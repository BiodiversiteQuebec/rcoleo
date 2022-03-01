
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
#'
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