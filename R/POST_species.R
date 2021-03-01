#' Publication d'une espèce sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `taxa` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_species <- function (data_list, ...) {
  post_gen_list(data_list, "taxa", ...)
}

