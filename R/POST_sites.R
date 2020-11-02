#' Publication d'un site sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `sites` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_sites <- function(data, ...) {

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoPostResp"

  endpoint <- endpoints()$sites

  for (i in 1:length(data)) {
    responses[[i]] <- post_gen(endpoint, data[[i]], ...)
  }

  return(responses)
}
