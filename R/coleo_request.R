###########################################################################
# functions in this script make GET requests for specific information from
# the database

# coleo_request_general runs any query on any table
# coleo_request_code only works for cells and sites
###########################################################################

#' Requête générale sur coleo de type 'GET'
#'
#' Des paramètres rendu disponibles via l'API peuvent être 'count', ou 'offset'.
#' @param endpoint Nom du endpoint de l'API de coleo sur lequel la requête doit être effectuée
#' @param perform TRUE par default. Retourne un objet httr2 request et pas de requête effectuée si FALSE.
#' @param response_as_df FALSE par défaut. Retroune un data.frame si TRUE.
#' @param limit Nombre de lignes par page. Valeur fixée par l'api de coleo
#' @param ... Paramètres de requête pour la base de données coleo (dans le format 'nom' = 'valeur')
#'
#' @return Object httr2 response si perform = TRUE et un tibble si response_as_df = TRUE, un objet httr2 request si perform = FALSE.
#' 
#' @examples
#' # Les requêtes sur des fonctions sont faites à rpc/FctName
#' species_data <- coleo2_request_general('rpc/taxa_richness',response_as_df = TRUE,'site_id_filter'=10, 'group_by_column'='campaign_type')
#' 
#' # Les requêtes sur des tables sont faites à TableName
#' sites <- coleo2_request_general('sites',response_as_df = TRUE,'schema'='public')

#' @export
coleo_request_general <- function(endpoint, perform = TRUE, response_as_df = FALSE, limit = 100,...){

  assertthat::assert_that(endpoint %in% names(endpoints()))

    request_info <- list(...)

  written_req <- coleo_begin_req() |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!request_info)

  if(perform) {
    out <- httr2::req_perform(written_req)

    if (response_as_df) out <- coleo_resp_df(out, written_req, limit)

    return(out)
  } else {
    return(written_req)
  }
}




#' Requête sur coléo par code
#'
#' Plusieurs entrées de la base de données sont des codes qui sont lisibles par
#' les humains. Les codes de site suivent le format "CCC_CC_LCC"
#' (C = chiffre, L=lettre) et les codes de cellules suivent le format "CCC_CC".
#' Cette fonction reçoit un de ces codes et retourne les informations de cette
#' entrée.
#'
#' @param human_code Code de site ou de cellule.
#' @param table Table de la base de données à accéder. Seuls "cells" et "sites" sont supportés pour l'instant.
#' @param perform TRUE par default. Ne performe pas la requête et retourne l'object httr2 request si FALSE.
#'
#' @return si perform = TRUE, la réponse est retournée. Si perform = FALSE, la requête httr2 est retournée.
#' @export
coleo_request_by_code <- function(human_code, table, perform = TRUE){
  # endpoint or whatever
  requested_code <- list(human_code)

  # could add a column here if a human-useable unique ID is used in any other table
  names(requested_code) <- switch(table,
                                  cells = "cell_code",
                                  sites = "site_code",
                                  stop("idk what to do with that"))

  written_req <- coleo_begin_req() |>
    httr2::req_url_path_append(table) |>
    httr2::req_url_query(!!!requested_code)

  if(isTRUE(perform)){
    httr2::req_perform(written_req)
  } else {
    return(written_req)
  }
}

#' Converti un objet httr2 response en un tibble.
#'
#' Si la réponse contient plusieurs pages, la fonction accède à chacune et les
#' joints en un seul data.frame
#'
#' @param resp objet httr2 response de coleo.
#' @param written_req Requête passée à l'API de coleo.
#' @param perform TRUE par défault. Ne performe pas la requête si FALSE.
#'
#' @return si perform = TRUE, la réponse est retournée. Si perform = FALSE, la requête httr2 est retournée.
#' @export
coleo_resp_df <- function(resp, written_req, limit){
  # is there pages?
  header_names <- names(resp$headers)

  if ("Content-Range" %in% header_names) {

    # Prep output object
    responses <- list()
    errors <- NULL

    # Get # pages
    tmp <- unlist(strsplit(resp$headers$"Content-Range", split = "\\D"))
    rg <- as.numeric(tmp[grepl("\\d", tmp)])
    pages <- rg[3L] %/% limit

    # Loop through pages
    for (page in 0:pages) {
      page_req <- written_req |>
        httr2::req_url_query(page = page)

      ## Get page response
      page_resp <- httr2::req_perform(page_req)

      ## Save response
      if (httr2::resp_is_error(page_resp)) {
        ## If error
        responses[[page + 1]] <- list(body = NULL, response = page_resp)
        errors <- append(errors, page + 1)
      } else {
        ## Save response as a tibble
        resp_as_df <- page_resp |>
          httr2::resp_body_json(simplifyVector = TRUE) |>
          tibble::as_tibble()
        responses[[page + 1]] <- list(body = resp_as_df, response = page_resp)
      }
    }

    # If error, print problematic pages
    if (!is.null(errors))
    warning("Failed request(s) for page(s): ", paste0(errors, ", "))

    # 
    out <- purrr::transpose(responses)
    out <- purrr::map_df(out$body, dplyr::bind_rows)
  } else {
    # No 'Content-Range'
    out <- httr2::resp_body_json(resp, simplifyVector = TRUE) |>
      tibble::as_tibble()
  }

  return(out)
}


#' Extract an id from an API response
#' Extraction de l'id d'une réponse de l'API de coleo
#'
#' Fonction de commodité pour processer les réponses de l'API de coleo.
#' Fonctionnelle pour les nouvelles entrées (réponses de requêtes POST) et
#' pour celles qui existent déjà (requêtes GET).
#'
#' @param answer_resp Réponse httr2 de l'API de coleo.
#'
#' @return Le id sous format d'integer ou \code{NA_integer_}
#' @export
coleo_extract_id <- function(answer_resp){
  ans_id <- answer_resp |>
    httr2::resp_body_json() |>
    # flatten might be safer than alternatives?
    purrr::flatten() |>
    purrr::pluck("id")

  # if there is no record in the DB, return NA rather than NULL
  if(is.null(ans_id)) return(NA_integer_) else return(ans_id)
}
