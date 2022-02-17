#' Fonction d'aide à l'obtention des tsn
#'
#' @param taxa nom scientifique des espèces
#' @param tsn_rows numérique, chiffre de 1 à infini. La valeur par défault NA spécifie que toutes les identifications
#'
#' @return
#' @export
#'
coleo_get_tsn <- function(taxa, tsn_rows = NA) {

  # Add tsn
  tsn <- as.numeric(taxize::get_tsn(taxa, rows = NA))

  # Problematic taxa names
  no_tsn <- taxa[is.na(tsn)]

  # print problematic rows
  which(taxa %in% no_tsn)

  return(tsn)
}
