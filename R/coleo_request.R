
#' General request for the coleo database
#'
#' @param ... query parameters for the coleo database (in the format `name = value`)
#' Possible parameters are 'count', and 'offset'.
#' @param endpoint name of API endpoint where this request should go
#' @param perform Should the request be performed? defaults to TRUE.
#' @param response_as_df Should the response be returned as a dataframe? defaults to FALSE.
#' @param limit Line number per pages. Parameter value fixed by coleo API.
#'
#' @return httr2 response object if perform = TRUE and a tibble if response_as_df = TRUE, a request object if perform = FALSE.
#' @export
coleo_request_general <- function(..., endpoint, perform = TRUE, response_as_df = FALSE, limit = 100){

  assertthat::assert_that(endpoint %in% names(endpoints()))

    request_info <- list(...)

  written_req <- coleo_begin_req() |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!request_info)

  if(perform) {
    out <- httr2::req_perform(written_req)

    if (response_as_df) out <- coleo_resp_df(out, written_req, limit)

    return(out)
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

#' Returns a request to a tibble
#'
#' If a response contains multiple pages, the function gets them all
#' and joins them in a single dataframe.
#'
#' @param resp httr2 response from coleo.
#' @param written_req request passed to coleo's api.
#' @param perform Should the request be performed? defaults to TRUE.
#'
#' @return if perform = TRUE, the answer is returned. if perform = FALSE, an httr2 query is returned.
#' @export
coleo_resp_df <- function(resp, written_req, limit){
  # is there pages?
  header_names <- names(resp$headers)

  if ("Content-Range" %in% header_names) {

    # Prep output object
    responses <- list()
    errors <- NULL

    # Get # pages
    tmp <- unlist(strsplit(resp$headers$"Content-Range", split = "\\D"))
    rg <- as.numeric(tmp[grepl("\\d", tmp)])
    pages <- rg[3L] %/% limit

    # Loop through pages
    for (page in 0:pages) {
      page_req <- written_req |>
        httr2::req_url_query(page = page)

      ## Get page response
      page_resp <- httr2::req_perform(page_req)

      ## Save response
      if (httr2::resp_is_error(page_resp)) {
        ## If error
        responses[[page + 1]] <- list(body = NULL, response = page_resp)
        errors <- append(errors, page + 1)
      } else {
        ## Save response as a tibble
        resp_as_df <- page_resp |>
          httr2::resp_body_json(simplifyVector = TRUE) |>
          tibble::as_tibble()
        responses[[page + 1]] <- list(body = resp_as_df, response = page_resp)
      }
    }

    # If error, print problematic pages
    if (!is.null(errors))
    warning("Failed request(s) for page(s): ", paste0(errors, ", "))

    # 
    out <- purrr::transpose(responses)
    out <- purrr::map_df(out$body, dplyr::bind_rows)
  } else {
    # No 'Content-Range'
    out <- httr2::resp_body_json(resp, simplifyVector = TRUE) |>
      tibble::as_tibble()
  }

  return(out)
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
