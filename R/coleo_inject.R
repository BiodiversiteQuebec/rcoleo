
#' General injection into the Coleo database
#'
#' @param endpoint The name of the endpoint you are injecting.
#' @param ... the data to be injected. May contain NA or NULL values; these will
#'   be dropped before injection.
#'
#' @return an httr2 request, ready to be performed
#' @export
#'
#' @examples coleo_inject_general("cells", cell_code = "444_33", name = "Middle Earth", geom = "")
coleo_inject_general <- function(endpoint, ...){

  endpt <- rcoleo:::endpoints()[[endpoint]]

  request_info <- list(...)

  # experimental! drop any NA or NULL arguments, since they do not need to be injected
  request_info <- request_info |>
    purrr::discard(.p = ~all(is.na(.x))) |>
    purrr::discard(.p = ~all(is.null(.x)))


  rcoleo:::coleo_begin_req() %>%
    httr2::req_url_path_append(endpt) %>%
    httr2::req_body_json(data = request_info)
}

