#' Publication d'un environnement dans l'API de coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `environments` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_environments <- function(data_list, ...) {
  post_gen_list(data_list, "environments", ...)
}

