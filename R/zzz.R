# Config de base
server <- function() "https://coleo.biodiversite-quebec.ca"
#server <- function() "http://localhost:8080" # dev purpose
base <- function() "/api/v1"
bearer <- function() ifelse(file.exists(".httr-oauth"), as.character(readRDS(".httr-oauth")), NA)
ua <- httr::user_agent("rcoleo")

# Point d'entrées de l'API
endpoints <- function(){
  list(
    cells = "/cells",
    sites = "/sites",
    campaigns = "/campaigns",
    media = "/media",
    observations = "/observations",
    taxa = "/taxa",
    landmarks = "/landmarks",
    attributes = "/attributes",
    traps = "/traps",
    samples = "/samples",
    lures = "/lures",
    devices = "/devices"
  )
}
