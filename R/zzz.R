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
    obs_soil_decomposition = "/obs_soil_decomposition",
    efforts = "/efforts",
    summary = "/campaigns_summary",
    species_list = "/species_list",
    sites_species = "/sites_species",
    richness = "/richness",
    richness_mean = "/richness/mean"
  )
}
# # short excerpt to update the list of campaign types if/when more get added!!
# ss <- download_sites_sf()
# purrr::keep(ss$campaigns, ~ nrow(.)>0) %>%
#   purrr::map("type") %>%
#   purrr::map(unique) %>%
#   purrr::flatten_chr() %>% unique %>% dput
campaign_types <- function(){
  c('v\u00e9g\u00e9tation', 'v\u00e9g\u00e9tation_transect', 'sol',
    'acoustique', 'ph\u00e9nologie', 'mammif\u00e8res', 'papilionid\u00e9s',
    'odonates', 'insectes_sol', 'ADNe', 'zooplancton', 'sol', 'd\u00e9composition_sol',
    'temp\u00e9rature_eau', 'temp\u00e9rature_sol', 'marais_profondeur_temp\u00e9rature')
}

site_types <- function(){
  c("lac", "rivi\u00e8re", "forestier", "marais", "marais c\u00f4tier", "toundrique", "tourbi\u00e8re")
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

