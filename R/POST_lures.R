#' Publication des leurres utilisés dans le cadre de la campagne dans l'API de coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `lures` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_lures <- function(data_list, ...) {
  post_gen_list(data_list, "lures", ...)
}

