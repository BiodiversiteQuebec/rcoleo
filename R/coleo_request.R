
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

# functions in this script make GET requests for specific information from the database

# coleo_request_general runs any query on any table
# coleo_request_code only works for cells and sites
# the other two are specific and should probably be deleted.


#' Request a database record by code
#'
#' many of the database records have unique human-readable codes. Site codes
#' look like "CCC_CC_LCC" (C = chiffre, L=lettre) and cell codes look like
#' "CCC_CC". this function takes one of these "human codes" and returns the
#' information on that record.
#'
#' @param human_code human-readable database code
#' @param table the database table to look for. Right now this is either "cells" or "sites"
#' @param perform Should the request be performed? defaults to TRUE.
#'
#' @return if perform = TRUE, the answer is returned. if perform = FALSE, an httr2 query is returned.
#' @export
coleo_request_by_code <- function(human_code, table, perform = TRUE){
  # endpoint or whatever
  requested_code <- list(human_code)

  # could add a column here if a human-useable unique ID is used in any other table
  names(requested_code) <- switch(table,
                                  cells = "cell_code",
                                  sites = "site_code",
                                  stop("idk what to do with that"))

  written_req <- coleo_begin_req() |>
    httr2::req_url_path_append(table) |>
    httr2::req_url_query(!!!requested_code)

  if(isTRUE(perform)){
    httr2::req_perform(written_req)
  } else {
    return(written_req)
  }
}


#' Extract an id from an API response
#'
#' this is a convenience function for processing the response from the COLEOs
#' api. It should work for both new records (the response from POST requests)
#' and existing ones (GET requests).
#'
#' @param answer_resp an HTTP response from the coleo API
#'
#' @return the ID as an integer or \code{NA_integer_}
#' @export
coleo_extract_id <- function(answer_resp){
  ans_id <- answer_resp |>
    httr2::resp_body_json() |>
    # flatten might be safer than alternatives?
    purrr::flatten() |>
    purrr::pluck("id")

  # if there is no record in the DB, return NA rather than NULL
  if(is.null(ans_id)) return(NA_integer_) else return(ans_id)
}



#' Download the site id
#'
#' @param site_code code pour le site, du format "123_92_F01"
#'
#' @return code for site (integer) or integer NA value
#' @export
coleo_get_site_id <- function(site_code){
  message(site_code)

  coleo_request_general(site_code = site_code, endpoint = "sites") |>
    coleo_pluck_one_id()
}

#' Download the cell id
#'
#' @param site_code code pour la cellule, du format "123_92"
#'
#' @return code for cell (integer) or integer NA value
#' @export
coleo_get_cell_id <- function(cell_code){
  message(cell_code)

  coleo_request_general(cell_code = cell_code, endpoint = "cells") |>
    coleo_pluck_one_id()
}

