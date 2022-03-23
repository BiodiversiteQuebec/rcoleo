#' Retourne un vecteur contenant les noms valides de campagnes
#'
#'
#' @return vecteur de charactères contenant tous les types de capagnes valides
#' @export
#'
coleo_return_valid_campaigns <- function(){
  campaigns <- coleo_get_enum_values("enum_campaigns_type")

  return(campaigns)
}


#' Retourne un vecteur contenant les noms valides de type de site
#'
#'
#' @return vecteur de charactères contenant tous les types de sites valides
#' @export
#'
coleo_return_valid_site_types <- function(){
  legal_vals <- coleo_get_enum_values("enum_sites_type")

  return(legal_vals)
}


#' Trouver les tables requises pour un type de capamgne donné
#'
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
