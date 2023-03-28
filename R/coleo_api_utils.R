# functions to make the foundation api calls, process errors etc

# Config de base
server <- function(){
  s <- Sys.getenv("COLEOAPI_SERVER")
  if (s==''){
    if(file.exists(".local-server")){
      s <- as.character(readRDS(".local-server"))
    }else{
      s <- "https://coleo.biodiversite-quebec.ca/newapi/v1"
    }
   }
   return(s)
 }


bearer <- function() {
  # ifelse(file.exists(".httr-oauth"), as.character(readRDS(".httr-oauth")), NA)
  token <- Sys.getenv("RCOLEO_TOKEN")
  if (token == "" & !file.exists(".httr-oauth2")) stop("Aucune autorisation d\u00e9tect\u00e9e")
  if (token == "") out <-  as.character(readRDS(".httr-oauth2")) else out <- token
  return(out)
}


# Return error message
coleo_error_message <- function(resp){
  resp_json <- resp |>
    httr2::resp_body_json()

  server_message <- resp_json$message
  error_message <- paste(resp_json$errors[[1]], collapse = ": ")

  return(paste0(server_message,
                ": ",
                error_message))
}

#' Pépare une requête générique à l'API de coleo
#'
#' @param schema Schéma sur lequel faire la requête
#'
#' @return Un objet httr2 request
coleo_begin_req <- function(schema) {
  server() |>
    httr2::request() |>
    httr2::req_headers("Accept" = "application/json",
                       `Content-Type` = "application/json",
                        "Content-Profile" = schema,
                       "Accept-Profile" = schema,
                       "Prefer" = "return=representation",
                       "Authorization" = paste("Bearer", bearer()),
                       "useragent" = "rcoleo") |>
    httr2::req_error(body = coleo_error_message)
}
