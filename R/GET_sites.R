#' Obtenir les informations sur les sites depuis l'API de coleo
#' @param site_code  `character` vecteur contenant les identifiants uniques que l'on désire obtenir. Si site_code n'est pas spécifié, la fonction retournera l'ensemble des entrées présentes dans la table cells.
#' @inheritParams get_gen
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' \dontrun{
#' get_sites(site_code=c('135_104_H01'))
#' head(get_sites())
#' }
#' @export
get_sites <- function(site_code = NULL, ...) {

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoGetResp"

  endpoint <- endpoints()$sites

  if (is.null(site_code)) {

    # Obtenir tous les sites
    responses[[1]] <- get_gen(endpoint, ...)

  } else {

    stopifnot(is.character(site_code))

    # Obtenir les sites définis dans ids
    for (id in 1:length(site_code)) {

      responses[[id]] <- get_gen(endpoint, query = list(site_code = site_code[id]), ...)

    }
  }


  return(responses)


}
