
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

  rcoleo:::coleo_begin_req() |>
    httr2::req_url_path_append(endpt) |>
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
#'
#' @export
coleo_inject_general_df <- function(df_one_row, endpoint) {

  df_one_row_ls <- as.list(df_one_row)

  # if there is a nested list, flatten.
    # this is a workaround for the behaviour of rowwise() when dealing with list-columns
    # Injections in coleo will be rejected if nested lists containing a single object are flattened
    # Technicians nested lists containing more than one object need to be flattened
  if (any(names(df_one_row_ls) %in% "technicians")) {
        n_techs <- df_one_row_ls["technicians"][[1]][[1]] |> length()
    if (n_techs > 1) {
      df_one_row_ls["technicians"] <- df_one_row_ls["technicians"][[1]]
    }
  }
    if (any(names(df_one_row_ls) %in% "geom")) {
      df_one_row_ls["geom"] <- df_one_row_ls["geom"][[1]]
    }

  df_one_row_ls$data <- NULL
  endpt <- rcoleo:::endpoints()[[endpoint]]


  # drop all NULL or NA columns
  df_one_row_ls[which(is.na(df_one_row_ls))] <- NULL


  rcoleo:::coleo_begin_req() |>
    httr2::req_url_path_append(endpt) |>
    httr2::req_body_json(data = df_one_row_ls)
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

  if(db_table == "observations") {
    colnames_of_tbl <- coleo_get_column_names(tbl = db_table)$column_name

    df_prep <- df |>
      coleo_prep_input_data(db_table) |>
      dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::across(dplyr::any_of(colnames_of_tbl)), endpoint = db_table)))

  } else if (db_table == "ref_species") {
    # ref_species is the only table where the table name and the endpoint name are NOT THE SAME
    # here we hard-code the difference. This lets us stay with the convention of using the table name as the argument (not the endpoint name)
    df_prep <- df |>
      coleo_prep_input_data(db_table) |>
      dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "taxa")))
  } else {
    df_prep <- df |>
      coleo_prep_input_data(db_table) |>
      dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = db_table)))

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
    dplyr::mutate(!!name_id := dplyr::if_else(is.null(error),
                                       true = coleo_extract_id(result),
                                       false = NA_integer_)
    )

  if(!newname %in% c("observation", "lure")) {
    df_out <- df_id |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::ends_with("id"), data) |>
      tidyr::unnest(cols = c(data))
  } else if(newname == "observation") {
    # once the observation table is injected, we only need observation_id. Here
    # is a cheesy dplyr way to do it -- you can't drop a col if it is a grouping
    # column ;)
    df_out <- df_id |>
      dplyr::ungroup() |>
      dplyr::group_by(observation_id) |>
      dplyr::relocate(dplyr::ends_with("id")) |>
      # dplyr::select(-dplyr::ends_with("id"), -inject_request, -result, -error, -success) |>
      dplyr::select(-inject_request, -result, -error, -success) |>
      dplyr::ungroup()
  } else {
    # lures table are processed back to wide format later
    # in coleo_inject_mam_lures()
    return(df_id)
  }

  return(df_out)
}


#' Inject data into coleo
#'
#' Takes a valid dataframe and performs autonomously the injection of all tables
#'
#' @param df a valid dataframe
#'
#' @return A message specifying the dataframe lines that were injected and that
#' failed to be injected, and the resulting dataframes from
#' coleo_injection_perform()
#' @export
coleo_inject <- function(df) {
  #--------------------------------------------------------------------------
  # 1. Extract tables to be injected
  #--------------------------------------------------------------------------
  campaign_type <- unique(df$campaigns_type)
  tables <- rcoleo::coleo_return_required_tables(campaign_type)

  #--------------------------------------------------------------------------
  # 2. inject campaigns table
  #--------------------------------------------------------------------------
  failures <- FALSE
  # Prep request
  campaigns_requests <- df |>
      rcoleo::coleo_injection_prep(db_table = "campaigns")
  # Requests executions
  campaigns_response <- coleo_injection_execute(campaigns_requests) # Real thing
  resp <- table(campaigns_response$success)

  # Output
  cat(paste0("\nInjection of campaigns table led to ", ifelse(is.na(resp['TRUE']), 0, resp['TRUE']), " successes, and ", ifelse(is.na(resp['FALSE']), 0, resp['FALSE']), " failures.\n"))

  if(!is.na(resp['FALSE'])) {
    cat("Only data for successfully injected campaigns are injected in the next tables. These following lines failed to inject: ", paste0(which(campaigns_response$success == FALSE), collapse = ", "), "\n")
    failures <- TRUE
    }

  # Get campaigns id
  df_id <- campaigns_response[campaigns_response$success == TRUE,] |>
      coleo_injection_final()

  #--------------------------------------------------------------------------
  # 3. Inject other tables
  #--------------------------------------------------------------------------
  for (table in tables[-1]) {

    # Case-specific injections
    if (campaign_type == "mammifères" & table == "lures") {
      ## Lures table for "mammifères" campaigns
      inject_ls <- coleo_inject_mam_lures(df_id, failures)
      df_id <- inject_ls[["df_id"]]
      failures <- inject_ls[["failures"]]

    } else if (campaign_type == "mammifères" & table == "landmarks") {
      ## Landmarks table for "mammifères" campaigns
      inject_ls <- coleo_inject_mam_landmarks(df_id, failures)
      df_id <- inject_ls[["df_id"]]
      failures <- inject_ls[["failures"]]

    } else if (campaign_type == "mammifères" & table == "media") {
      # Inject medias in coleo before injecting into media table

    } else if (table == "media") {
      ## The special case of media files
      ### 1. Inject media files into coleo
      inject_ls <- coleo_inject_media(df_id, failures, server_dir = 'observation')
      df_id <- inject_ls[["df_id"]]
      failures <- inject_ls[["failures"]]

      ### 2. Then inject media table
      df_id <- coleo_inject_table(df_id, failures, table)

    } else {
      ## regular table injections
      df_id <- coleo_inject_table(df_id, failures, table)
    }
  }
  return(failures)
}

# Helper function to return stadardized injection reponse messages
injection_reponse_message <- function(table, response, failures) {
  resp <- table(response$success)
  # Output
  cat(paste0("\nInjection of ",table, " table lead to ", ifelse(is.na(resp['TRUE']), 0, resp['TRUE']), " successes, and ", ifelse(is.na(resp['FALSE']), 0, resp['FALSE']), " failures.\n"))

  if(!is.na(resp['FALSE'])) {
    cat("These lines failed to inject: ", dput(which(response$success == FALSE)), "\n")
    ## It is expected that not all ref_species are injected
    ## - Each taxon is only injected once
    if(table != "ref_species") failures <- TRUE
    }
  return(failures)
}


#' Inject tables into coleo
#'
#' Takes a valid dataframe and performs autonomously the injection of data into appropriate table
#'
#' @param df_id a dataframe with *_id column(s)
#' @param failures Boolean. Failures within the injection process
#' @param table a coleo table name
#'
#' @return a dataframe with lure ids
#'
coleo_inject_table <- function(df_id, failures, table) {
  #--------------------------------------------------------------------------
  # 1. Prep request
  #--------------------------------------------------------------------------
  requests <- df_id |>
      rcoleo::coleo_injection_prep(db_table = table)

  #--------------------------------------------------------------------------
  # 2. Requests executions
  #--------------------------------------------------------------------------
  response <- coleo_injection_execute(requests) # Real thing
  # Rename device_id in mammifères campaigns
  # - Prevents landmarks injection of lures to inject device_id
  if(campaign_type == "mammifères" & table == "devices") names(df_id)[names(df_id) == "device_id"] <- "device_id_camera"

  #--------------------------------------------------------------------------
  # 3. Print output
  #--------------------------------------------------------------------------
  # Output
  failures <- injection_reponse_message(table, response, failures)

  #--------------------------------------------------------------------------
  # 4. Get id
  # - Failure will cause an error
  # - Only get id for successful injections
  #--------------------------------------------------------------------------
  if(any(response$success)) {
    response_t <- response[response$success == TRUE,]
    response_f <- response[response$success == FALSE,]
    df_id <- response_t |>
      coleo_injection_final()
    response_f[setdiff(names(df_id), names(response_f))] <- NA
    rbind(df_id, response_f[,names(df_id)])
  }
  return(df_id)
}


#' Injection of media files into the Coleo server
#'
#' @param dir The name of the directory you are injecting (campaign, observation, site).
#' @param id The id generated by coleo for the table row to link to the file. Can be the observation_id, the campaign_id, or the site_id.
#' @param file_path The local path to the media file to inject.
#' @param failures Boolean. Failures within the injection process.
#'
#' @return an httr2 request, ready to be performed.
#'
#' @export
coleo_inject_media <- function(df_id, failures, server_dir = "observation", file_dir) {
  #--------------------------------------------------------------------------
  # 1. Prep request
  #--------------------------------------------------------------------------
  url <- paste0(server(), "/upload/", server_dir, "/")

  medias_requests <- df_id |>
    dplyr::rowwise() |>
    dplyr::mutate(inject_request = list(
      ## Form request
      httr2::request(url) |>
        httr2::req_headers(
          "Content-Type" = "multipart/form-data",
          "Authorization" = paste("Bearer", bearer())
        ) |>
        httr2::req_url_path_append(!!as.name(paste0(server_dir, "_id"))) |>
        ## Error body
        #httr2::req_error(body = coleo_error_message) |>
        ## Send file
        httr2::req_body_file(path = paste0(file_dir, "/", media_name), type = "image")
    ))

  #--------------------------------------------------------------------------
  # 2. Injection
  #--------------------------------------------------------------------------
  medias_requests$inject_request[[1]] |> httr2::req_dry_run()

  response <- coleo_injection_execute(medias_requests)

  response$error[[1]]



#===========================================
#========= TEST  WITH STEVE CODE ===========
#===========================================
# USING HTTR
r <- httr::POST(
  "https://coleo.biodiversite-quebec.ca/upload/campaign/11016",
  body = list(
    type = "image",
    media = httr::upload_file("./97_90_F01_R439-03_20200612_004813.JPG")
  ),
  config = httr::add_headers(
    `Content-type` = "multipart/form-data",
    Authorization = paste("Bearer", bearer())
  )
)
#===========================================
#===========================================
#===========================================



  #--------------------------------------------------------------------------
  # 3. Print output
  #--------------------------------------------------------------------------
  # Output
  failures <- injection_reponse_message("media file", response, failures)

  #--------------------------------------------------------------------------
  # 3. Get uuid back
  #--------------------------------------------------------------------------
  if (any(response$success)) {
    response_t <- response[response$success == TRUE, ]
    response_f <- response[response$success == FALSE, ]
    df_id <- df |>
      dplyr::mutate(uuid := dplyr::if_else(is.null(error),
        true = coleo_extract_id(result),
        false = NA_integer_
      ))
    response_f[setdiff(names(df_id), names(response_f))] <- NA
    rbind(df_id, response_f[, names(df_id)])
    df_id <- df_id |>
      dplyr::select(-inject_request, -result, -error, -success)
  }

  return(list("df_id" = df_id, "failures" = failures))
}


#' Inject lures table of mammifère campaigns into coleo
#'
#' Takes a valid dataframe and performs autonomously the injection of lures table
#'
#' @param df_id a dataframe with campaign_id column
#' @param failures Boolean. Failures within the injection process
#'
#' @return a dataframe with lure ids
#'
coleo_inject_mam_lures <- function(df_id, failures) {
  #--------------------------------------------------------------------------
  # 1. Format data
  #--------------------------------------------------------------------------
  luresCols <- names(df_id)[grepl("lures", names(df_id))]
  ## There might be multiple lures (1 to 5)
  lures_col_groups <- split(luresCols, sub('.*(?=.$)', "", luresCols, perl = TRUE))
  ## df to long format
  df_long <- data.frame("campaign_id" = as.numeric(), "installed_at" = as.character(), "lure" = as.character())
  for(group in lures_col_groups) {
      df_long <- mapply(c, df_long, df_id[,c("campaign_id", group)]) |> as.data.frame()
  }
  df_long <- df_long[complete.cases(df_long[,-1]),]
  #--------------------------------------------------------------------------
  # 2. Prep requests
  #--------------------------------------------------------------------------
  df_prep <- df_long |>
        dplyr::rowwise() |>
        dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "lures")))
  #--------------------------------------------------------------------------
  # 3. Injection
  #--------------------------------------------------------------------------
  response <- coleo_injection_execute(df_prep) # Real thing
  # Output
  failures <- injection_reponse_message("lures", response, failures)
  #--------------------------------------------------------------------------
  # 4. Finalizing lures table injection
  #--------------------------------------------------------------------------
  if(any(response$success)) {
    response_t <- response[response$success == TRUE,]
    response_f <- response[response$success == FALSE,]
    lures_id <- response_t |>
      coleo_injection_final()
    response_f[setdiff(names(lures_id), names(response_f))] <- NA
    rbind(lures_id, response_f[,names(lures_id)])
  }

  # Reassign lure_id to df_id
  # - lure_ids are saved in separate columns for each lure
  # - using the format lure_id_1, lure_id_2, etc
  for(lure_row in seq_along(lures_id$campaign_id)) {
      camp = lures_id[lure_row, "campaign_id"] |> unlist()
      date = lures_id[lure_row, "installed_at"] |> unlist()
      ## Which row
      df_row <- which(camp == df_id$campaign_id)
      ## Which col
      instal_cols <- df_id[,grepl("lures_installed_at_", names(df_id))]
      instal_dates <- t(instal_cols[duplicated(instal_cols),])
      df_col <- which(date == instal_dates)
      ## Save lure_id for right lure
      lure_no <- sub('.*(?=.$)', "", names(instal_cols)[df_col], perl = TRUE)
      df_id[df_row, paste0("lure_id_", lure_no)] <- lures_id$lure_id[lure_row]
  }
  return(list("df_id" = df_id, "failures" = failures))
}


#' Inject landmarks table of mammifère campaigns into coleo
#'
#' Takes a valid dataframe and performs autonomously the injection of landmarks table
#'
#' @param df_id a dataframe with campaign_id column
#' @param failures Boolean. Failures within the injection process
#'
#' @return a dataframe with landmarks ids
#'
coleo_inject_mam_landmarks <- function(df_id, failures) {
  #--------------------------------------------------------------------------
  # 1. Format data
  #--------------------------------------------------------------------------
  landCols <- names(df_id)[grepl("landmarks", names(df_id))]
  # There might be multiple landmarks for the camera and for each lure
  land_groups <- split(landCols, sub('^.*_([a-zA-Z]+$)', "\\1", landCols, perl = TRUE))
  #--------------------------------------------------------------------------
  # 2. Inject camera landmark
  #--------------------------------------------------------------------------
  # Remove _camera from colnames
  df_camera <- df_id
  names(df_camera)[grep("_camera", names(df_camera))] <- sub('_camera', "\\1", names(df_camera)[grep("_camera", names(df_camera))], perl = TRUE)
  # Prep requests
  requests <- df_camera |>
    rcoleo::coleo_injection_prep(db_table = "landmarks")
  # Injection
  response <- coleo_injection_execute(requests) # Real thing
  # Output
  failures <- injection_reponse_message("landmark_camera", response, failures)
  # Finalizing lures table injection
  if(any(response$success)) {
    response_t <- response[response$success == TRUE,]
    response_f <- response[response$success == FALSE,]
    df_id <- response_t |>
      coleo_injection_final()
    response_f[setdiff(names(df_id), names(response_f))] <- NA
    rbind(df_id, response_f[,names(df_id)])
  }
  names(df_id)[names(df_id) == "landmark_id"] <- "landmark_id_camera"
  #--------------------------------------------------------------------------
  # 3. Inject one landmark for each lure_id
  #--------------------------------------------------------------------------
  # 1. get lure groups
  lures_ids <- names(df_id)[grepl("lure_id", names(df_id))]
  lures <- split(lures_ids, sub('.*(?=.$)', "", lures_ids, perl = TRUE))
  # 2. loop through groups
  for(lureGroup in lures) {
    ## 2.1. rename landmarks columns and lure_id for the group
    ## this will allow coleo_prep_data to know exactly which columns to use for injection
    df_lure <- df_id
    names(df_lure)[grep("_appat", names(df_lure))] <- sub('_appat', "\\1", names(df_lure)[grep("_appat", names(df_lure))], perl = TRUE)
    names(df_lure)[names(df_lure) == lureGroup] <- "lure_id"
    ## 2.2. Prep requests
    requests <- df_lure |>
        rcoleo::coleo_injection_prep(db_table = "landmarks")
    ## 2.2. Inject
    response <- coleo_injection_execute(requests) # Real thing
    ### Output
    land_col_name <- paste0("landmark_id_appat_", sub('.*(?=.$)', "", lureGroup, perl = TRUE))
    failures <- injection_reponse_message(land_col_name, response, failures)
    ## 2.3. Save landmarks_id in df_id and rename it
    if(any(response$success)) {
      response_t <- response[response$success == TRUE,]
      response_f <- response[response$success == FALSE,]
      df_appat_final <- response_t |>
        coleo_injection_final()
      response_f[setdiff(names(df_appat_final), names(response_f))] <- NA
      rbind(df_appat_final, response_f[,names(df_appat_final)])
    }
    df_id$landmark_id <- df_appat_final$landmark_id
    names(df_id)[names(df_id) == "landmark_id"] <- land_col_name
  }
  return(list("df_id" = df_id, "failures" = failures))
}