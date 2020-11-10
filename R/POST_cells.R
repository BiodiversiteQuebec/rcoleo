#' Publication d'une cellule sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `cells` de l'API de Coleo
#'
#' @param data une liste ou chacun des niveaux corresponds aux données attribuées au ednpoint.
#' @param ... httr options; arguments de la fonction `httr::POST()`
#' @return Un objet \code{list}, dont chacun des niveaux corresponds à la réponse de l'API. La réponse peut être de classe \code{postError} ou \code{postSuccess}.
#' @seealso \code{\link{post_gen}} pour la structure de sortie de la fonction.
#' @export
post_cells <- function (data)
{
  responses <- list()
  status_code <- NULL
  class(responses) <- "coleoPostResp"
  endpoint <- endpoints()$cellules

  for (i in 1:length(data)) {
    responses[[i]] <- rcoleo::post_gen(endpoint, data[[i]])
    status_code <- c(status_code, responses[[i]]$response$status_code)
  }

  if(all(status_code == 201)){
    print("Good job ! Toutes les insertions ont été crées dans COLEO")
  }else{
    print("Oups... un problème est survenu")
    print(status_code)
  }
  return(responses)

}
