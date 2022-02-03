# functions that work with coleo_get_required_tables()

#' Trouver les tables requises pour un type de capagne donné
#'
#' @param camp_type un type de campagne valide.
#'
#' @return
#' @export
coleo_return_required_tables <- function(camp_type) {

  # test that camp_type is valid choice

  full_tbl <- coleo_get_required_tables()

  tbls <- full_tbl[full_tbl[,camp_type]==1, "table"][[1]]

  return(tbls)
}
