#' Publication d'un site sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `sites` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_sites <- function (data_list, ...) {
  post_gen_list(data_list, "sites", ...)
}

