# functions that work with coleo_get_required_tables()

#' Trouver les tables requises pour un type de capagne donné
#'
#' @param camp_type un type de campagne valide.
#'
#' @return
#' @export
coleo_return_required_tables <- function(camp_type) {

  full_tbl <- coleo_get_required_tables()

  # test that camp_type is valid choice
  assertthat::assert_that(camp_type %in% names(full_tbl)[-1],
                          msg = "Not one of the database campaign type")

  tbls <- full_tbl[full_tbl[,camp_type]==1, "table"][[1]]

  return(tbls)
}
