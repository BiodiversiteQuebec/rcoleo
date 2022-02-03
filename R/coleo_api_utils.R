# functions to make the foundation api calls, process errors etc

coleo_error_message <- function(resp){
  resp_json <- resp %>%
    httr2::resp_body_json(.)

  server_message <- resp_json %>% .$message
  error_message <- resp_json %>% .$errors %>% purrr::map_chr("message") %>% paste(collapse = "; ")
  return(paste0(server_message,
                ": ",
                error_message))
}

# create a generic request
coleo_begin_req <- function(){
  paste0(rcoleo:::server(), rcoleo:::base()) |>
    httr2::request() |>
    httr2::req_headers("Accept" = "application/json",
                       `Content-Type` = "application/json",
                       "Authorization" = paste("Bearer", rcoleo:::bearer()),
                       "useragent" = "rcoleo") |>
    httr2::req_error(body = coleo_error_message)
}
