#' Publication d'une campagne sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `campaigns` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_campaigns <- function (data_list, ...){
  post_gen_list(data_list, "campaigns", ...)
}
