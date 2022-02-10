#' Valide la forme du jeu de données à injecter
#'
#' @param data jeu de données à valider
#'
#' @return
#' @export
#'
coleo_validate <- function(data) {

  # Check that there is a campaign type column and that it contains a unique value
  assertthat::assert_that(assertthat::has_name(data, "campaign_type"),
                          msg = "Vérifiez qu'une colonne contient le type de campagne et que son nom de colonne correspond à campaign_type")

  campaign_type <- unique(data$campaign_type)
  assertthat::assert_that(length(campaign_type) == 1 && campaign_type %in% coleo_return_valid_campaigns(),
                          msg = "Vérifiez que toutes les valeurs de la colonne campaign_type sont identiques et que la valeur est un type de campagne valide")

  # Check that imported data has a column called site_code
  # This is necessary since the sites table's fields within input columns are not required to inject a data set, but the "site_code" input column is
  assertthat::assert_that(assertthat::has_name(data, "site_code"),
                          msg = "Le jeu de données ne contient pas de colonne nommée site_code. Vérifiez qu'une colonne contient le code du site et que le nom de colone correspond à site_code")

  # Check that the imported data has all of the required columns
  req_tbls <- coleo_return_required_tables(campaign_type)
  req_columns <- c(unlist(sapply(req_tbls, coleo_get_required_name_table)),
                   "site_code") # site_code needs to be manually added
  req_col_diff <- setdiff(req_columns, names(data))

  assertthat::assert_that(length(req_col_diff) == 0,
                          msg = paste0("Le jeu de données ne contient pas toutes les colonnes requises pour être injecté. Vérifiez que les bons noms de colonnes sont utilisés et que toutes les colonnes sont présentes. Les colonnes requises sont : \n", paste0(req_columns, collapse = ", "), "\n\nLes colonnes absentes sont : \n", paste0(req_col_diff, collapse = ", ")))

  # Check that all input columns are valid column names
  columns <- c(unlist(sapply(req_tbls, coleo_get_name_table_column)),
               "site_code") # site_code needs to be manually added
  possible_col_diff <- setdiff(names(data), columns)

  assertthat::assert_that(length(possible_col_diff) == 0,
                          msg = paste0("Le jeu de données contient des colonnes au nom invalide. Vérifiez que les bons noms de colonnes sont utilisés. Les colonnes qui sont superflues doivent être retirées. Les colonnes valides peuvent être : \n", paste0(columns, collapse = ", "), "\n\nLes colonnes au nom invalide sont : \n", paste0(possible_col_diff, collapse = ", ")))

  # Check that input column types are valid
  tbl <- coleo_get_name_table()
  dat_names <- names(data)

  ## Check that all values within each column is of the right class
  class_of_col <- sapply(dat_names, function(x) {
    class_of_col_values <- sapply(data[,x], tbl$required_class[[which(tbl$input_column==x)]])
    all(class_of_col_values)
    })

  ## Are all input columns of the right class?
  erroneous_cols <- dat_names[!class_of_col]
  assertthat::assert_that(all(class_of_col),
                          msg = paste0("Le jeu de données contient des colonnes de classe invalide. Vérifiez la classe des colonnes. Ces colonnes sont problématiques : \n", paste0(erroneous_cols, collapse = ", ")))

  # Check that the range of values contained within input columns are valid
  tbl_with_legal_values <- subset(tbl, !is.na(legal_values))
  cols_to_check <- intersect(dat_names, tbl_with_legal_values$input_column)

  valid_col_values <- sapply(cols_to_check, function(x) {
    legal_vals <- tbl$legal_values[which(tbl$input_column==x)][[1]]
    all(unique(data[,x]) %in% legal_vals)
    })

  assertthat::assert_that(all(valid_col_values),
                          msg = paste0("Le jeu de données contient des colonnes dont les valeurs ne sont pas valides. Ces colonnes sont problématiques : \n", paste0(names(valid_col_values)[!valid_col_values], collapse = ", ")))



  # Check that the format of the input column date is valid
  ## Andrew's idea: [0-9]{4}-[0-9]{2}-[0-9]{2}
  ## strsplit(date_col, “-“)


}
