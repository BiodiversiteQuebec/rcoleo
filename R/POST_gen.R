#' Fonction générique pour envoyer de l'information vers l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données.
#' @param singleton Données à injecter à l'intérieur de la base de données.
#' @param token  `character` jeton d'accès pour authentification auprès de l'API
#' @param ... httr options; arguments de la fonction `httr::POST()`
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à la réponse de l'API. Pour chacun des appels POST sur l'API, la classe retourné est `postSuccess` ou `postError`. Une réponse de classe `postSuccess` ou `postError` est une liste à deux niveaux composée du `body`, et de la réponse, voir [httr::response]. Si le réponse est de classe `postSucces`, le `body` contiendra les données injectées sinon il contiendra le message d'erreur retourné par l'API.
#' @export

post_gen <- function(endpoint, singleton, token = bearer(), ...) {

  if (!exists("endpoint")) {
    stop("Le point d'acc\u00e8s aux donn\u00e9es est manquant (ex. /cells)")
  }

  url <- httr::modify_url(server(), path = paste0(base(), endpoint))

  resp <- httr::POST(url, body = jsonlite::toJSON(singleton, auto_unbox = TRUE),
    config = httr::add_headers(`Content-type` = "application/json", Authorization = paste("Bearer",
      token)), ua, ...)

  if (resp$status == 401) {

    structure(list(body = httr::http_status(resp), response = resp), class = "postError")

  } else if (resp$status == 400) {

    structure(list(body = jsonlite::fromJSON(httr::content(resp, "text")), response = resp),
      class = "postError")

  } else if (resp$status == 201) {

    structure(list(body = jsonlite::fromJSON(httr::content(resp, "text")), response = resp), class = "postSuccess")

  } else if (resp$status == 500) {

    structure(list(body = jsonlite::fromJSON(httr::content(resp, "text")), response = resp),
      class = "postError")

  }
}

#' Fonction générique pour envoyer de l'information vers l'API de Coléo
#'
#' @param data_list list contenant des donnees a injecter. voir [post_gen]
#' @param endpoint Point d'entrée pour le retrait des données.
#' @param ... httr options; arguments de la fonction `httr::POST()`
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API.
#' @export

post_gen_list <- function(data_list, endpoint, ...){

  assertthat::assert_that(endpoint %in% names(endpoints()))

  responses <- purrr::list_along(data_list)

  class(responses) <- "coleoPostResp"

  endpoint <- endpoints()[[endpoint]]

  for (i in 1:length(responses)) {
    responses[[i]] <- post_gen(endpoint, data_list[[i]], ...)
  }

  check_post_list(responses)
  return(responses)

}
