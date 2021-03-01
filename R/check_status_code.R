# check the status code in a response

check_status_code <- function(response) {

  assertthat::assert_that(assertthat::has_name(response, "response"))
  # get the status code from the responses
  status_code <- httr::status_code(response$response)

  return(status_code)


}


check_post_list <- function(response_list){

  # if (is.null(names(response_list))) message("consider naming your list")

  resp_codes <- sapply(response_list, check_status_code)

  if(all(resp_codes == 201)){
    message("Good job ! Toutes les insertions ont \u00e9t\u00e9 cr\u00e9\u00e9es dans COLEO")
  }else{
    message(paste("Oups! une probl\u00e8me est survenu:",
                  paste(resp_codes, collapse = ", ")))
  }


}
