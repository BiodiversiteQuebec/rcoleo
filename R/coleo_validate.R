#' Title
#'
#' @param data jeu de données à valider
#' @param campaign_type type de campagne à injecter.
#'
#' @return
#' @export
#'
coleo_validate <- function(data) {

  # Check that there is a campaign type column and that it contains a unique value
  assertthat::assert_that(assertthat::has_name(data, "camp_type"),
                          msg = "Erreur: La colonne camp_type est manquante du jeu de donnée. Vérifiez qu'une colonne contient le type de campagne et que son nom de colonne correspond à camp_type")

  campaign_type <- unique(data$camp_type)
  assertthat::assert_that(length(campaign_type) == 1 & campaign_type %in% coleo_return_valid_campaigns(),
                          msg = "Erreur: Les valeurs contenus dans la colonne camp_type ne sont pas valides. Vérifiez que toutes les valeurs de la colonne sont identiques et que la valeur est un type de campagne valide")

  # Check that imported data has a column called site_code
  # This is necessary since the sites table's fields within input columns are not required to inject a data set, but the "site_code" input column is
  assertthat::assert_that(assertthat::has_name(data, "site_code"),
                          msg = "Erreur: Le jeu de données ne contient pas de colonne nommée site_code. Vérifiez qu'une colonne contient le code du site et que le nom de colone correspond à site_code")

  # Check that the imported data has all of the required columns
  ## Required tables for the campaign type


  ## Required column names for the required tables


  ## Check that all required column names are present in the input dataset
  setdiff(correct_names, injection_data_names)

  # Check that all input columns are valid column names


  # Check that all input column correspond to fields needed to inject that specific campaign type


  # Check that input column types are valid
  tbl$required_class[[1]](tbl$column[i])

  # Check that the format of the input column date is valid


  # Check that the range of values contained within input columns are valid






  # function to check the campaigns!

  req_tables <- coleo_return_required_tables("insectes_sol")

  # get the columns from those tables

  #yyyy-mm-dd

  stop("wrong name")

  warning("")

  setdiff(correct_names, injection_data_names)

  setdiff(c("a", "n", "d"), letters)

}
