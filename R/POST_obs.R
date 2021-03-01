#' Publication des observations d'une campagne sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `observations` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_obs <- function(data_list, ...) {
  post_gen_list(data_list, "observations", ...)
}

