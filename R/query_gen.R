#' query one endpoint and get a dataframe
#'
#' @param endpoint the name of the endpoint
#' @param ... extra arguements to get_gen
#'
#' @return a data.frame of endpoint answers
#' @export
query_gen <- function(endpoint, ...) {

  assertthat::assert_that(
    endpoint %in% names(endpoints())
  )


  api_response <- get_gen(endpoints()[[endpoint]], ...)


  if ( !is.data.frame(api_response)) {

    assertthat::assert_that( identical(names(api_response), c("body", "response")) )


    api_response <- dplyr::bind_rows(api_response[["body"]])
  }


  return(api_response)

}
