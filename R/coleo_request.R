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


#' General request for the coleo database
#'
#' @param ... query parameters for the coleo database (in the format `name = value`)
#' @param table name of database table to query
#' @param perform Should the request be performed? defaults to TRUE.
#'
#' @return httr2 response object if perform = TRUE, a request object if perform = FALSE.
#' @export
coleo_request_general <- function(..., endpoint, perform = TRUE){

  assertthat::assert_that(endpoint %in% names(endpoints()))

    request_info <- list(...)

  written_req <- coleo_begin_req() |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!request_info)

  if(isTRUE(perform)){
    httr2::req_perform(written_req)
  } else {
    return(written_req)
  }
}


coleo_pluck_one_id <- function(answer_resp){
  answer_resp |>
    httr2::resp_body_json() |>
    # flatten might be safer than alternatives?
    purrr::flatten() |>
    purrr::pluck("id")
}


coleo_get_site_id <- function(site_code){
  message(site_code)

  coleo_request_general(site_code = site_code, endpoint = "sites") |>
    coleo_pluck_one_id()
}

