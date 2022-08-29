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
    obs_edna = "/obs_edna",
    summary = "/campaigns_summary",
    species_list = "/species_list",
    sites_species = "/sites_species",
    richness = "/richness",
    richness_mean = "/richness/mean",
    table_columns = "/table_columns",
    enum_options = "/enum_options"
  )
}
# # short excerpt to update the list of campaign types if/when more get added!!
# ss <- download_sites_sf()
# purrr::keep(ss$campaigns, ~ nrow(.)>0) |>
#   purrr::map("type") |>
#   purrr::map(unique) |>
#   purrr::flatten_chr() |> unique |> dput
campaign_types <- function(){
  camp_types_resp <- coleo_request_general(enum = "enum_campaigns_type", endpoint = "enum_options")
  unlist(httr2::resp_body_json(camp_types_resp), use.names = FALSE)
}

site_types <- function(){
  camp_types_resp <- coleo_request_general(enum = "enum_sites_type", endpoint = "enum_options")
  unlist(httr2::resp_body_json(camp_types_resp), use.names = FALSE)
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

