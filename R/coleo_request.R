###########################################################################
# functions in this script make GET requests for specific information from
# the database

# coleo_request_general runs any query on any table, view or function
# coleo_request_code only works for cells and sites
###########################################################################

#' Requête générale sur coleo de type 'GET'
#'
#' Cette fonction permet de faire une requête sur l'API de coleo. Les requêtes de type GET se
#' font sur le schéma api.
#' 
#' @param endpoint Nom du endpoint de l'API de coleo sur lequel la requête doit être effectuée. Si 
#' la requête est faite sur une fonction, il est nécessaire d'ajouter 'rpc/' devant le nom de la fonction.
#' @param perform TRUE par default. Retourne un objet httr2 request et pas de requête effectuée si FALSE.
#' @param response_as_df FALSE par défaut. Retroune un data.frame si TRUE.
#' @param schema Schema qui contient les fonctions ou tables de l'appel.
#' @param page_size Nombre d'items par page. Par défaut, 10000.
#' @param ... Paramètres de requête pour la base de données coleo (dans le format 'nom' = 'valeur')
#'
#' @return Liste d'objects httr2 response si perform = TRUE et un tibble si response_as_df = TRUE, un objet httr2 request si perform = FALSE.
#' @export
#' 
#' @examples
#' # Requête sur la table 'cells'
#' coleo_request_general('cells', perform = FALSE, response_as_df = TRUE, schema = 'public')
#' 
#' # Requête sur la fonction 'table_columns'
#' coleo_request_general('rpc/table_columns', perform = FALSE, response_as_df = TRUE, 
#' 'table_name' = 'cells')
#' 
coleo_request_general <- function(endpoint, perform = TRUE, response_as_df = FALSE, schema = 'api', page_size = 10000, ...){

  request_info <- list(...)

  written_req <- coleo_begin_req(schema) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!request_info)

  if(perform) {
    all_data <- list()
    start <- 0

    # Pagination
    repeat {
      written_req <- written_req |>
        httr2::req_headers(Range = sprintf("%d-%d", start, start + page_size - 1))
      resp <- httr2::req_perform(written_req)
      data <-  httr2::resp_body_json(resp, simplifyVector = TRUE)
      all_data <- append(all_data, list(data))

      content_range <- httr2::resp_headers(resp)[["content-range"]]
      total <- as.integer(sub(".*/", "", content_range))
      cat(sprintf("\rProgression: %d%%", round(start / total * 100)))
      start <- start + page_size
      if (start >= total) break    
    }
    cat("\rProgression: 100%\n")

    # Combine data from all pages
    if (response_as_df) {
      all_data_df <- do.call(rbind, lapply(all_data, function(x) as.data.frame(x))) |>
        tibble::as_tibble()
      
      return(all_data_df)
    }

    return(all_data)
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
#' @param schema Schéma de la base de données à accéder. Par défaut, "public".
#' @param perform TRUE par default. Ne performe pas la requête et retourne l'object httr2 request si FALSE.
#'
#' @return si perform = TRUE, la réponse est retournée. Si perform = FALSE, la requête httr2 est retournée.
#' @export
coleo_request_by_code <- function(human_code, table, schema = "public", perform = TRUE){
  # endpoint or whatever
  requested_code <- list(human_code)
  requested_code <- paste0("eq.", requested_code[[1]])

  # could add a column here if a human-useable unique ID is used in any other table
  names(requested_code) <- switch(table,
                                  cells = "cell_code",
                                  sites = "site_code",
                                  remote_sensing_indicators = "name",
                                  stop("idk what to do with that"))

  written_req <- coleo_begin_req(schema) |>
    httr2::req_url_path_append(table) |>
    httr2::req_url_query(!!!requested_code)

  if(isTRUE(perform)){
    httr2::req_perform(written_req)
  } else {
    return(written_req)
  }
}


#' Requête de type 'GET' sur les tables de données par type d'inventaire de coleo
#'
#' Cette fonction permet de faire une requête sur les tables de données de l'API de coleo.
#' 
#' @param survey_type Type d'inventaire. Les types d'inventaires supportés sont 'vegetation', 'acoustique_anoures', 'acoustique_chiropteres', 'acoustique_oiseaux', 'acoustique_orthopteres' et 'adne_corrige', 'benthos', 'decomposition_sol', 'insectes_sol', 'mammiferes', 'odonates', 'papilionides', 'physicochimie_terrain', 'zooplancton'.
#' @param view 'short' par défaut. Type de vue des données à retourner. 'short' retourne les données de base, 'long' retourne les données dans le format gabarit d'injection (requiert accès prévilégier).
#' @param ... Paramètres de requête pour la base de données coleo (dans le format 'nom' = 'valeur')
#'
#' @return Tibble contenant les données de l'inventaire.
#' @export
#' 
#' @examples
#' # Requête pour les inventaires de type 'végétation'
#' coleo_request_data(survey_type = 'vegetation', view = 'short')
#' 
coleo_request_data <- function(survey_type, view = 'short', ...){

  request_info <- list(...)

  # endpoint
  endpoint <- paste0('gabarit_', survey_type, '_', view)

  out <- coleo_request_general(endpoint, perform = TRUE, response_as_df = TRUE, schema = 'api', ...)

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