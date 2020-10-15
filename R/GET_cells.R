#' Obtenir les informations sur les cellules depuis l'API de coleo
#' @param cell_code `character` vecteur contenant les identifiants uniques que l'on désire obtenir. Si cell_code n'est pas spécifié, la fonction retournera l'ensemble des entrées présentes dans la table cells.
#' @param name `character` vecteur contenant les noms de la cellules que l'on veut obtenir.
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @inheritParams get_gen
#' @examples
#' \dontrun{
#' get_cells(cell_code=c('111_91'))
#' get_cells()
#' }
#' @export

get_cells <- function(cell_code = NULL,name = NULL, ...) {

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoGetResp"

  endpoint <- endpoints()$cells

  if (is.null(cell_code) & is.null(name)) {

    # Obtenir tous les sites
    responses[[1]] <- get_gen(endpoint, ...)

  } else {

    # tests args to set iterator
    len <- max(c(length(cell_code),length(name)))

    # Obtenir les sites définis dans ids
    for (id in 1:len) {

      # prep query
      responses[[id]] <- get_gen(endpoint, query = list(
        cell_code = cell_code[id],
        name = name[id]
      ), ...)

    }
  }


  return(responses)


}
