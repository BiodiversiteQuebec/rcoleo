# functions to make the foundation api calls, process errors etc

# Config de base
server <- function(){
  s <- Sys.getenv("COLEOAPI_SERVER")
  if (s==''){
    if(file.exists(".local-server")){
      s <- as.character(readRDS(".local-server"))
    }else{
      s <- "https://coleo.biodiversite-quebec.ca"
    }
   }
   return(s)
 }

#server <- function() "http://localhost:8080" # dev purpose
base <- function() {
  path <- Sys.getenv("COLEOAPI_PATH")
  if (path==''){
    path="/newapi/v2"
  }
  return(path)
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
coleo_begin_req <- function(schema){
  paste0(server(), base()) |>
    httr2::request() |>
    httr2::req_headers("Accept" = "application/json",
                       `Content-Type` = "application/json",
                        "Content-Profile" = schema,
                       "Accept-Profile" = schema,
                       "Prefer" = "return=representation",
                       "Authorization" = paste("Bearer", bearer()),
                       "useragent" = "rcoleo",
                       "Prefer" = "count=exact",
                       "Range-Unit" = "items") |>
    httr2::req_error(body = coleo_error_message)
}


####################
# Fonctions de base pour les médias
####################

 # Config de base pour les médias
media_server <- function(){
  s <- Sys.getenv("COLEOMEDIA_SERVER")
  if (s==''){
    if(file.exists(".local-server")){
      s <- as.character(readRDS(".local-server"))
    }else{
      s <- "https://biodiversite-quebec.ca/upload/"
    }
   }
   return(s)
 }

#' Pépare une requête générique à l'API de coleo-media
#'
#' @param server_dir Dossier sur le serveur où faire la requête
#'
#' @return Un objet httr2 request
coleo_media_begin_req <- function(server_dir){
  paste0(media_server(), server_dir, "/") |>
    httr2::request() |>
    httr2::req_headers("Accept" = "application/json",
                       `Content-type` = "multipart/form-data",
                       "Authorization" = paste("Bearer", bearer())) |>
    httr2::req_error(body = coleo_error_message)
}

#' Déclencheur de l'API plumber pour mettre à jour les données du portail
#' 
#' Cette fonction déclenche la mise à jour des données du portail.
#' 
#' @return Void. Ouvre le navigateur à l'endpoint de mise à jour.
coleo_plumber_update <- function(){
  browseURL("https://coleo.biodiversite-quebec.ca/r-update-api/update/no")
}

