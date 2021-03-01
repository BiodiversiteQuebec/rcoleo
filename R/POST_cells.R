#' Publication d'une cellule sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `cells` de l'API de Coleo
#'
#' @param data_list une liste ou chacun des niveaux corresponds aux données attribuées au ednpoint.
#' @param ... httr options; arguments de la fonction `httr::POST()`
#' @return Un objet \code{list}, dont chacun des niveaux corresponds à la réponse de l'API. La réponse peut être de classe \code{postError} ou \code{postSuccess}.
#' @seealso \code{\link{post_gen}} pour la structure de sortie de la fonction.
#' #' @seealso \code{\link{post_gen_list}} pour la fonction generique.
#' @export
post_cells <- function (data_list, ...) {
  post_gen_list(data_list, "cells", ...)
}
