#' Publication d'attribut(s) dans la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `attributes` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_attributes <- function(data_list, ...) {
  post_gen_list(data_list, "attributes", ...)
}
