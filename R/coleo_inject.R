
#' General injection into the Coleo database
#'
#' @param endpoint The name of the endpoint you are injecting.
#' @param ... the data to be injected. May contain NA or NULL values; these will
#'   be dropped before injection.
#'
#' @return an httr2 request, ready to be performed
#' @export
coleo_inject_general <- function(..., endpoint){

  if(is.null(endpoint)) stop("Need to specify the endpoint")


  request_info <- list(...)

  # experimental! drop any NA or NULL arguments, since they do not need to be injected
  request_info <- request_info |>
    purrr::discard(.p = ~all(is.na(.x))) |>
    purrr::discard(.p = ~all(is.null(.x)))


  endpt <- rcoleo:::endpoints()[[endpoint]]

  rcoleo:::coleo_begin_req() %>%
    httr2::req_url_path_append(endpt) %>%
    httr2::req_body_json(data = request_info)
}




#' Inject one dataframe row
#'
#' A "tidy" workflow keeps all the data together in one list
#'
#' @param df_one_row one row of the dataframe. This can be passed in with
#'   \code{\link[dplyr]{cur_data_all}}
#' @param endpoint endpoint for coleo DB. Should name the table where you want
#'   to inject this.
#'
#' @return A single HTTP POST request. this is NOT run. You have to run it
#'   manually. First, you should inspect the contents with
#'   \code{\link[httr2]{req_dry_run}}
#' @export
coleo_inject_general_df <- function(df_one_row, endpoint){

  df_one_row <-as.list(df_one_row)

  df_one_row$data <- NULL
  endpt <- rcoleo:::endpoints()[[endpoint]]


  # drop all NULL or NA columns
  df_one_row[which(is.na(df_one_row))] <- NULL


  rcoleo:::coleo_begin_req() %>%
    httr2::req_url_path_append(endpt) %>%
    httr2::req_body_json(data = df_one_row)
}


#' Inject data into coleo
#'
#' Takes a dataframe with a column of httr2 POST requests and actually performs
#' them
#'
#' @param df a dataframe with one (and only one) column containing http requests
#'   to inject data
#'
#' @return the same dataframe, with three new columns: the http response, the
#'   error message (if any) and a column \code{success} which is TRUE if that
#'   row was successfully injected
#' @export
coleo_injection_execute <- function(df){


    which_req <- which(sapply(df, \(x) class(x[[1]])) == "httr2_request")

    if(length(which_req) != 1) stop("you must have only one request column")


    df_result <- df |>
      dplyr::mutate(inject_result = list(
        purrr::safely(httr2::req_perform)(.data[[names(which_req)]])
      ))

    df_result |>
      dplyr::mutate(result = list(inject_result$result),
             error = list(inject_result$error),
             success = is.null(error)) |>
      dplyr::select(-inject_result)

}



#' Prepare dataset for injection
#'
#' Take a dataset and prepare the data for injection into a
#'
#' @param df dataframe containing injection material.
#' @param db_table the database table into which the data will be injected
#'
#' @return the same dataframe, but with a new column for injection request. Note
#'   that the dataframe will be \code{rowwise} after this function
#' @export
coleo_injection_prep <- function(df, db_table){

  # prep the data by nesting unneeded columns, renaming those remaining, and adding these to a request

  # the exception is the observation table, where we should NOT nest. instead we
  # rename and make the request using the columsn which the table accepts (if
  # any of these are present)

  # unless it is observations

  if(db_table != "observations") {
    df_prep <- df |>
      coleo_prep_input_data(db_table) |>
      dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = db_table)))

    # if its observations
  } else {
    colnames_of_tbl <- coleo_get_column_names(tbl = db_table)$column_name

    df_prep <- df |>
      coleo_prep_input_data(db_table) |>
      dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::across(dplyr::any_of(colnames_of_tbl)), endpoint = db_table)))

  }

  return(df_prep)

}

#' Finalize coleo injection
#'
#' After successful injection, process the dataframe: extract the new ID from
#' the injected records, store it in a new column, and drop all the injected
#' columns
#'
#' @param df dataframe produced by \code{coleo_injection_execute}
#'
#' @return a data.frame. All the injected columns are gone, and it has no groups
#'   or nested data. the ID of the records just injected is stored in a column
#'   with the correct name
#' @export
coleo_injection_final <- function(df){
  # get the name of the type of table just injected and make a name_id out of it.
  newname <- df$inject_request[[1]] |>
    httr2::req_dry_run(quiet = TRUE) |>
    purrr::pluck("path") |>
    basename() |>
    sub(pattern = "s$", replacement = "")

  name_id <- paste0(newname, "_id")


  # ALMOST offensively fashionable way to dynamically name a column
  df_id <- df |>
    dplyr::mutate(!!name_id := coleo_extract_id(result))

  if(newname != "observation") {
    df_out <- df_id |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::ends_with("id"), data) |>
      tidyr::unnest(cols = c(data))
  } else {
    df_out <- df_id |>
      dplyr::ungroup() |>
      dplyr::relocate(dplyr::ends_with("id")) |>
      dplyr::select(-inject_request, -result, -error, -success)

  }

  return(df_out)
}
