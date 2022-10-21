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
#' @param schema Schema qui contient les fonctions ou tables de l'appel
#' @param ... Paramètres de requête pour la base de données coleo (dans leformat 'nom = valeur')
#'
#' @return Object httr2 response si perform = TRUE et un tibble si response_as_df = TRUE, un objet httr2 request si perform = FALSE.
#' @export
coleo2_request_general <- function(endpoint, perform = TRUE, response_as_df = FALSE, limit = 100, schema='api'...){

  assertthat::assert_that(endpoint %in% names(endpoints()))

    request_info <- list(...)

  written_req <- coleo2_begin_req(schema) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!request_info)

  if(perform) {
    out <- httr2::req_perform(written_req)

    if (response_as_df) out <- coleo2_resp_df(out, written_req, limit)

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
coleo2_request_by_code <- function(human_code, table, perform = TRUE){
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
coleo2_resp_df <- function(resp, written_req, limit){
  # is there pages?
  header_names <- names(resp$headers)
    # No 'Content-Range'
    out <- httr2::resp_body_json(resp, simplifyVector = TRUE) |>
      tibble::as_tibble()
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
coleo2_extract_id <- function(answer_resp){
  ans_id <- answer_resp |>
    httr2::resp_body_json() |>
    # flatten might be safer than alternatives?
    purrr::flatten() |>
    purrr::pluck("id")

  # if there is no record in the DB, return NA rather than NULL
  if(is.null(ans_id)) return(NA_integer_) else return(ans_id)
}


# create a generic request
coleo2_begin_req <- function(schema){
  paste0(rcoleo:::server2(), rcoleo:::base2()) |>
    httr2::request() |>
    httr2::req_headers("Accept" = "application/json",
                       `Content-Type` = "application/json",
                        "Content-Profile"= schema,
                       "Accept-Profile"= schema,
                       "Authorization" = paste("Bearer", rcoleo:::bearer2()),
                       "useragent" = "rcoleo") |>
    httr2::req_error(body = coleo_error_message)
}

