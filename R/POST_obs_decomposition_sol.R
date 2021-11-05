#' Publication d'une observation de décomposition du sol sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `obs_soil_decomposition` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_obs_decomposition_sol <- function(data_list, ...) {
  post_gen_list(data_list, "obs_soil_decomposition", ...)
}

