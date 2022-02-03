# functions in this script make GET requests for specific information from the database

# coleo_request_general runs any query on any table
# coleo_request_code only works for cells and sites
# the other two are specific and should probably be deleted.


coleo_request_code <- function(human_code, table, perform = TRUE){
  # endpoint or whatever
  requested_code <- list(human_code)

  names(requested_code) <- switch(table,
                                  cells = "cell_code",
                                  sites = "site_code",
                                  stop("idk what to do with that"))

  written_req <- coleo_begin_req() %>%
    httr2::req_url_path_append(table) |>
    httr2::req_url_query(!!!requested_code)

  if(isTRUE(perform)){
    httr2::req_perform(written_req)
  } else {
    return(written_req)
  }
}


coleo_request_general <- function(..., table, perform = TRUE){
  # endpoint or whatever
  request_info <- list(...)

  written_req <- coleo_begin_req() %>%
    httr2::req_url_path_append(table) |>
    httr2::req_url_query(!!!request_info)

  if(isTRUE(perform)){
    httr2::req_perform(written_req)
  } else {
    return(written_req)
  }
}

