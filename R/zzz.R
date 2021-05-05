# Config de base
server <- function() ifelse(file.exists(".local-server"), as.character(readRDS(".local-server")), "https://coleo.biodiversite-quebec.ca")
#server <- function() "http://localhost:8080" # dev purpose
base <- function() "/api/v1"

bearer <- function() {
  # ifelse(file.exists(".httr-oauth"), as.character(readRDS(".httr-oauth")), NA)
  token <- Sys.getenv("RCOLEO_TOKEN")
  if (token == "" & !file.exists(".httr-oauth")) stop("Aucune autorisation d\u00e9tect\u00e9e")
  if (token == "") out <-  as.character(readRDS(".httr-oauth")) else out <- token
  return(out)
}



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
    environments = "/environment",
    attributes = "/attributes",
    traps = "/traps",
    samples = "/samples",
    lures = "/lures",
    devices = "/devices",
    obs_species = "/obs_species",
    efforts = "/efforts",
    summary = "/campaigns_summary",
    species_list = "/species_list"
  )
}



# PRINT/MESSAGES HELPERS

empty_line <- function() {
  message(paste(rep(" ", getOption("width") - 3), collapse = ""), "\r",
          appendLF = FALSE)
}


msg_request_fail <- function(resp) {
  sta <- httr::status_code(resp)
  warning("API request failed: error ", sta)
}

