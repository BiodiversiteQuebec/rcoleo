
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
#' the injected records, and store it in a new column
#'
#' @param df dataframe produced by \code{coleo_injection_execute}
#'
#' @return a data.frame. Without groups
#'   or nested data. the ID of the records just injected is stored in a column
#'   with the correct name
#' @export
coleo_injection_final <- function(df){
  #--------------------------------------------------------------------------
  # 1. Get the name of the type of table just injected and make a name_id out of it
  #--------------------------------------------------------------------------
  tbl_name <- df$inject_request[[1]] |>
    httr2::req_dry_run(quiet = TRUE) |>
    purrr::pluck("path") |>
    basename()
  # Media is a special case, because the POST is done on a server rather than on a table
  # - The injection in media table and obs_media are automated
  # - The table taxa has a different endpoint name (ref_species)
  if (!is.na(suppressWarnings(as.numeric(tbl_name)))) tbl_name <- "media"
  newname <- sub(tbl_name, pattern = "s$", replacement = "")
  if(tbl_name == "taxa") tbl_name = newname = "ref_species"

  name_id <- paste0(newname, "_id")
  name_err <- paste0(newname, "_error")

  #--------------------------------------------------------------------------
  # 2. Store id and error to new columns
  #--------------------------------------------------------------------------
  df_id <- df |>
    dplyr::mutate(!!name_id := ifelse(is.null(.data$error),
                                       coleo_extract_id(result),
                                       NA_integer_)
    ) |>
    dplyr::mutate(!!name_err := list(error))

  #--------------------------------------------------------------------------
  # 3. Rename table columns to keep
  #--------------------------------------------------------------------------
  # Select column names to keep
  old_names <- coleo_get_column_names(tbl_name)$column_name
  old_names <- old_names[!grepl("_id", old_names)]
  which_old <- names(df_id) %in% old_names
  # Make new column names
  new_names <- paste0(tbl_name, "_", names(df_id)[which_old])
  # Change column names to new names
  names(df_id)[which_old] <- new_names

  #--------------------------------------------------------------------------
  # 4. Remove the columns we don't want to keep and unnest the data
  # - lures are to be formated later, the table is returned as is
  #--------------------------------------------------------------------------
  if(!newname %in% c("observation", "media", "lure")) {
    df_out <- df_id[,order(colnames(df_id))] |>
      dplyr::ungroup() |>
      dplyr::select(-success, -inject_request, -error, -result) |>
      tidyr::unnest(cols = c(data)) |>
      dplyr::relocate(dplyr::ends_with("_error")) |>
      dplyr::relocate(dplyr::ends_with("_id"))
  } else if(newname %in% c("observation", "media")) {
    # There is no "data" column in observation or media tables
    df_out <- df_id[,order(colnames(df_id))] |>
      dplyr::relocate(dplyr::ends_with("_error")) |>
      dplyr::relocate(dplyr::ends_with("_id")) |>
      dplyr::select(-inject_request, -error, -success, -result)
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
#' @param media_path local path to media files. Required when media files are to be injected, NULL otherwise
#'
#' @return A message specifying the dataframe lines that were injected and that
#' failed to be injected, and the resulting dataframes from
#' coleo_injection_perform()
#' @export
coleo_inject <- function(df, media_path = NULL) {
  #--------------------------------------------------------------------------
  # 1. Extract tables to be injected
  #--------------------------------------------------------------------------
  campaign_type <- unique(df$campaigns_type)
  tables <- rcoleo::coleo_return_required_tables(campaign_type)

  #--------------------------------------------------------------------------
  # 2. Inject campaigns table
  #--------------------------------------------------------------------------
  # Prep request
  campaigns_requests <- df |>
      rcoleo::coleo_injection_prep(db_table = "campaigns")
  # Requests executions
  campaigns_response <- rcoleo::coleo_injection_execute(campaigns_requests) # Real thing
  resp <- table(campaigns_response$success)

  # Output
  cat(paste0("\nInjection of campaigns table led to ", ifelse(is.na(resp['TRUE']), 0, resp['TRUE']), " successes, and ", ifelse(is.na(resp['FALSE']), 0, resp['FALSE']), " failures.\n"))

  if(!is.na(resp['FALSE'])) {
    cat("Only data for successfully injected campaigns are injected in the next tables. These following lines failed to inject: ", paste0(which(campaigns_response$success == FALSE), collapse = ", "), "\n")
    }

  # Get campaigns id
  df_id <- rcoleo::coleo_injection_final(campaigns_response)

  #--------------------------------------------------------------------------
  # 3. Inject other tables
  #--------------------------------------------------------------------------
  for (table in tables[-1]) {

    # Injection of taxa_name in ref_species table
    if (table %in% c("landmarks", "obs_species", "obs_edna")) {
      if (campaign_type != "mammifères" & table != "landmarks") {
        # Skip if landmarks table in a mammifère campaign.
        # The landmarks need first to be extracted
        # - The operation is done in coleo_inject_mam_landmarks()
        taxa_col <- df_id[,paste0(table, "_taxa_name")] |>
          as.data.frame()
        coleo_inject_ref_species(taxa_col)
      }
    }

    # Case-specific injections
    if (campaign_type == "mammifères" & table == "lures") {
      ## Lures table for "mammifères" campaigns
      df_id <- coleo_inject_mam_lures(df_id)

    } else if (campaign_type == "mammifères" & table == "landmarks") {
      ## Landmarks table for "mammifères" campaigns
      df_id <- coleo_inject_mam_landmarks(df_id)

    } else if (table == "media") {
      ## The special case of media files
      ### 0. Check that path to media files is provided
      if (is.null(media_path)) stop("The local path to media files is missing. Media files and table could not be injected")
      ### 1. Inject media files into coleo
      df_id <- coleo_inject_media(df_id, server_dir = 'observation', media_path)

    } else if (campaign_type == "ADNe" & table == "landmarks") {
      ## Obervations at the lake scale for "ADNe" campaigns do not have landmarks
      ## Landmarks are injected even if there is no data with NA lat lon
      ## It is necessary to skip their injection
      df_id <- coleo_inject_adne_landmarks(df_id, campaign_type)

    } else {
      ## Regular table injections
      df_id <- coleo_inject_table(df_id, campaign_type, table)
    }
  }

  return(df_id)
}

# Helper function to return stadardized injection reponse messages
injection_reponse_message <- function(table, response) {
  resp <- table(response$success)
  # Output
  cat(paste0("\nInjection of ",table, " table led to ", ifelse(is.na(resp['TRUE']), 0, resp['TRUE']), " successes, and ", ifelse(is.na(resp['FALSE']), 0, resp['FALSE']), " failures.\n"))

  if(!is.na(resp['FALSE'])) {
    cat("These lines failed to inject: ", dput(which(response$success == FALSE)), "\n")
    ## It is expected that not all ref_species are injected
    ## - Each taxon is only injected once
    }
}


#' Inject tables into coleo
#'
#' Takes a valid dataframe and performs autonomously the injection of data into appropriate table
#'
#' @param df_id a dataframe with *_id column(s)
#' @param campaign_type Type of campaign the data is from
#' @param table a coleo table name
#'
#' @return a dataframe with lure ids
#'
coleo_inject_table <- function(df_id, campaign_type, table) {
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
  injection_reponse_message(table, response)

  #--------------------------------------------------------------------------
  # 4. Get id
  # - Failure will cause an error
  # - Only get id for successful injections
  #--------------------------------------------------------------------------
  df_out <- response |>
    coleo_injection_final()

  return(df_out)
}


#' Injection of media files into the Coleo server
#'
#' @param df_id A dataframe with *_id column(s)
#' @param dir The name of the directory you are injecting (campaign, observation, site).
#' @param file_path The local path to the media file to inject.
#'
#' @return an httr2 request, ready to be performed.
#'
#' @export
coleo_inject_media <- function(df_id, server_dir = "observation", file_dir) {
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
          `Content-type` = "multipart/form-data",
          Authorization = paste("Bearer", bearer())
        ) |>
        ## Add id to request URL
        ## - Can be observation_id, campaign_id, etc.
        httr2::req_url_path_append(!!as.name(paste0(server_dir, "_id"))) |>
        ## Error body
        httr2::req_error(body = coleo_error_message) |>
        ## Send file
        httr2::req_body_multipart(list(
          media = curl::form_file(paste0(file_dir, "/", media_name)),
          type = "image"))
    ))

  #--------------------------------------------------------------------------
  # 2. Injection
  #--------------------------------------------------------------------------
  response <- coleo_injection_execute(medias_requests)

  #--------------------------------------------------------------------------
  # 3. Print output
  #--------------------------------------------------------------------------
  # Output
  injection_reponse_message("media files and media", response)

  #--------------------------------------------------------------------------
  # 4. Get id back
  # - From the response, get the id of the media file
  #--------------------------------------------------------------------------
  df_id <- response |>
    coleo_injection_final()

  return(df_id)
}


#' Inject lures table of mammifère campaigns into coleo
#'
#' Takes a valid dataframe and performs autonomously the injection of lures table
#'
#' @param df_id a dataframe with campaign_id column
#'
#' @return a dataframe with lure ids
#'
coleo_inject_mam_lures <- function(df_id) {
  #--------------------------------------------------------------------------
  # 1. Format data
  #--------------------------------------------------------------------------
  luresCols <- names(df_id)[grepl("lures", names(df_id))]
  ## There might be multiple lures (1 to 5)
  lures_col_groups <- split(luresCols, sub('.*(?=.$)', "", luresCols, perl = TRUE))
  ## df to long format
  df_long <- data.frame("campaign_id" = as.numeric(), "installed_at" = as.character(), "lure" = as.character())
  for(group in lures_col_groups) {
    df_group <- df_id[,c("campaign_id", group)]
    names(df_group) <- names(df_long)
    df_long <- rbind(df_long, df_group)
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
  injection_reponse_message("lures", response)
  #--------------------------------------------------------------------------
  # 4. Finalizing lures table injection
  #--------------------------------------------------------------------------
  lures_id <- response |>
    coleo_injection_final()

  # Reassign lure_id to df_id
  # - lure_ids are saved in separate columns for each lure
  # - using the format lure_id_1, lure_id_2, etc
  for(lure_row in seq_along(lures_id$campaign_id)) {
      camp = lures_id[lure_row, "campaign_id"] |> unlist()
      date = lures_id[lure_row, "lures_installed_at"] |> unlist()
      ## Which row
      df_row <- which(camp == df_id$campaign_id)
      ## Which col
      instal_cols <- df_id[,grepl("lures_installed_at_", names(df_id))]
      instal_dates <- t(instal_cols[!duplicated(instal_cols),])
      df_col <- which(date == instal_dates)
      ## Save lure_id for right lure
      lure_no <- sub('.*(?=.$)', "", names(instal_cols)[df_col], perl = TRUE)
      id_col_name <- paste0("lure_", lure_no, "_id")
      if (id_col_name %in% colnames(df_id)) {
        # If column already exists, add to it
        df_id[df_row, id_col_name] <- lures_id$lure_id[lure_row]
      } else {
        # If column doesn't exist, create it
        df_id[df_row, ncol(df_id) + 1] <- lures_id$lure_id[lure_row]
        names(df_id)[ncol(df_id)] <- id_col_name
      }
      
      ## Save lure_result
      err_col_name <- paste0("lure_", lure_no, "_error")
      if (err_col_name %in% colnames(df_id)) {
        # If column already exists, add to it
        df_id[df_row, err_col_name] <- list(lures_id$lure_error[lure_row])
      } else {
        # If column doesn't exist, create it
        df_id[df_row, ncol(df_id) + 1] <- list(lures_id$lure_error[lure_row])
        names(df_id)[ncol(df_id)] <- err_col_name
      }
  }

  # Order columns
  df_id <- df_id[,order(colnames(df_id))] |>
      dplyr::relocate(dplyr::ends_with("_error")) |>
      dplyr::relocate(dplyr::ends_with("_id"))

  return(df_id)
}


#' Inject landmarks table of mammifère campaigns into coleo
#'
#' Takes a valid dataframe and performs autonomously the injection of landmarks table
#'
#' @param df_id a dataframe with campaign_id column
#'
#' @return a dataframe with landmarks ids
#'
coleo_inject_mam_landmarks <- function(df_id) {
  #--------------------------------------------------------------------------
  # 1. Format data
  #--------------------------------------------------------------------------
  landCols <- names(df_id)[grepl("landmarks", names(df_id))]
  # There might be multiple landmarks for the camera and for each lure
  land_groups <- split(landCols, sub('^.*_([a-zA-Z]+$)', "\\1", landCols, perl = TRUE))
  #--------------------------------------------------------------------------
  # 2. Inject taxa names in ref_species table for camera landmark
  #--------------------------------------------------------------------------
  if ("landmarks_taxa_name_camera" %in% names(df_id)) {
    coleo_inject_ref_species(df_id[,"landmarks_taxa_name_camera"])
  }
  #--------------------------------------------------------------------------
  # 3. Inject camera landmark
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
  injection_reponse_message("landmark_camera", response)
  # Finalizing lures table injection
  df_id <- response |>
    coleo_injection_final()

  # Rename camera columns
  which_camera <- grepl("landmarks", names(df_id)) & !grepl("_appat", names(df_id))
  names(df_id)[which_camera] <- paste0(names(df_id)[which_camera], "_camera")

  names(df_id)[names(df_id) == "landmark_id"] <- "landmark_camera_id"
  names(df_id)[names(df_id) == "landmark_error"] <- "landmark_camera_error"
  #--------------------------------------------------------------------------
  # 4. Inject one landmark for each lure_id
  #--------------------------------------------------------------------------
  # 1. get lure groups
  lures_ids <- names(df_id)[grepl("lure_._id", names(df_id))]
  lures <- split(lures_ids, grep("[[:digit:]]+", lures_ids))
  # 2. loop through groups
  for(lureGroup in lures) {
    ## 2.1. rename landmarks columns and lure_id for the group
    ## this will allow coleo_prep_data to know exactly which columns to use for injection
    df_lure <- df_id
    names(df_lure)[grep("_appat", names(df_lure))] <- sub('_appat', "\\1", names(df_lure)[grep("_appat", names(df_lure))], perl = TRUE)
    names(df_lure)[names(df_lure) == lureGroup] <- "lure_id"
    ## 2.2. Inject taxa_names in ref_species table for lure landmark
    if ("landmarks_taxa_name" %in% names(df_lure)) {
      coleo_inject_ref_species(df_lure[,"landmarks_taxa_name"])
    }
    ## 2.3. Prep requests
    requests <- df_lure |>
        rcoleo::coleo_injection_prep(db_table = "landmarks")
    ## 2.4. Inject
    response <- coleo_injection_execute(requests) # Real thing
    ## 2.4.1. Output
    land_id_name <- paste0("landmark_appat_", gsub('.*_([0-9]+)_.*', '\\1', lureGroup), "_id")
    land_error_name <- paste0("landmark_appat_", gsub('.*_([0-9]+)_.*', '\\1', lureGroup), "_error")
    injection_reponse_message(land_id_name, response)
    ## 2.5. Save landmarks_id in df_id and rename it
    df_appat_final <- response |>
      coleo_injection_final()
    df_id$landmark_id <- df_appat_final$landmark_id
    df_id$landmark_error <- df_appat_final$landmark_error

    names(df_id)[names(df_id) == "landmark_id"] <- land_id_name
    names(df_id)[names(df_id) == "landmark_error"] <- land_error_name
  }

  # Order columns
  df_id <- df_id[,order(colnames(df_id))] |>
      dplyr::relocate(dplyr::ends_with("_error")) |>
      dplyr::relocate(dplyr::ends_with("_id"))

  return(df_id)
}


#' Inject obs_edna table of ADNe campaigns into coleo
#'
#' Takes a valid dataframe and performs autonomously the injection of obs_edna table
#'
#' @param df_id a dataframe with campaign_id column
#'
#' @return a dataframe with landmarks ids
#'
coleo_inject_adne_landmarks <- function(df_id, campaign_type) {
  # Obervations at the lake scale for "ADNe" campaigns do not have landmarks
  # Landmarks are injected even if there is no data with NA lat lon
  # It is necessary to skip their injection
  which_lac <- df_id$observations_extra_value_1 == "lac"
  no_lake_id <- df_id[!which_lac,] |>
    coleo_inject_table(campaign_type = campaign_type, table = "landmarks")

  # Reassemble the dataframes
  with_lac <- df_id[which_lac,]
  with_lac[setdiff(names(no_lake_id), names(with_lac))] <- NA_character_
  df_id <- rbind(no_lake_id, with_lac[names(no_lake_id)])

  # Order columns
  df_id <- df_id[,order(colnames(df_id))] |>
      dplyr::relocate(dplyr::ends_with("_error")) |>
      dplyr::relocate(dplyr::ends_with("_id"))

  # Return the results
  return(df_id)
}


#' Inject taxa_names in ref_species table of coleo
#'
#' Takes a vector of taxa_names and autonomously attempts to
#' inject them in ref_species table.
#' 
#' This function is silent and won't print any messages
#' as all taxa_names will be POSTed to the server, but all
#' taxons already present will return an error message.
#'
#' @param taxa_col a dataframe with one column of taxa_names
#'
coleo_inject_ref_species <- function(taxa_col) {
  # Remove duplicated names
  taxa_col <- as.data.frame(taxa_col)
  taxa_col <- taxa_col[!duplicated(taxa_col),] |>
    as.data.frame()
  colnames(taxa_col) <- "ref_species_name"

  if (length(taxa_col) > 0) {
    taxa_col |>
      rcoleo::coleo_injection_prep(db_table = "ref_species") |>
      rcoleo::coleo_injection_execute()
  }
}