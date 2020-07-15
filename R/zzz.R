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



# PRINT/MESSAGES HELPERS

empty_line <- function() {
  message(paste(rep(" ", getOption("width") - 3), collapse = ""), "\r",
          appendLF = FALSE)
}


msg_request_fail <- function(resp) {
  sta <- httr::status_code(resp)
  warning("API request failed: error ", sta)
}


# handle_query <- function(query, names_available) {
#   if (is.character(query)) return(list(q = query))
#   if (!is.list(query))
#     stop("`query` should either be a list or a character string.",
#          call. = FALSE)
#   if (length(query) > 1) {
#     warning("Only the first element of the list is considered.", call. = FALSE)
#     query <- query[1L]
#   }
#   if (! names(query) %in% names_available)
#     stop("Only ", paste(names_available, collapse = ", "),
#          " are valid names for custom queries.", call. = FALSE)
#   query
# }
# # Remove ==> other message "should be named"
#
