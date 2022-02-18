#' Valide la forme du jeu de données à injecter
#'
#' @param data jeu de données à valider
#'
#' @return
#' @export
#'
coleo_validate <- function(data) {

  # Check that there is a campaign type column and that it contains a unique value
  if(!assertthat::has_name(data, "campaign_type")) stop("Vérifiez qu'une colonne contient le type de campagne et que son nom de colonne correspond à campaign_type. \nLe type de campagne est nécessaire pour les prochaines étapes de validation.\n")

  campaign_type <- unique(data$campaign_type)
  #  assertthat::assert_that(length(campaign_type) == 1 && campaign_type %in% coleo_return_valid_campaigns(),
  #                          msg = "Vérifiez que toutes les valeurs de la colonne campaign_type sont identiques et que la valeur est un type de campagne valide.\n")
  if(!(length(campaign_type) == 1 && campaign_type %in% coleo_return_valid_campaigns())) stop("Vérifiez que toutes les valeurs de la colonne campaign_type sont identiques et que la valeur est un type de campagne valide. \nLe type de campagne est nécessaire pour les prochaines étapes de validation.\n")

  # Check that imported data has a column called site_code
  # This is necessary since the sites table's fields within input columns are not required to inject a data set, but the "site_code" input column is
  if(!assertthat::has_name(data, "site_code")) warning("Vérifiez qu'une colonne contient le code du site et que le nom de colone correspond à site_code.\n")

  # Check that the imported data has all of the required columns
  ## compare required column names to present columns
  req_tbls <- coleo_return_required_tables(campaign_type)
  req_columns <- c(unlist(sapply(req_tbls, coleo_get_required_name_table)),
                   "site_code") # site_code needs to be manually added
  req_col_diff <- setdiff(req_columns, names(data))
  ## Return warning if there's a mismatch
  if(length(req_col_diff) != 0) warning("Vérifiez que les bons noms de colonnes sont utilisés et que toutes les colonnes requises sont présentes. Les colonnes requises sont : \n", paste0(req_columns, collapse = ", "), "\n\nLes colonnes absentes sont : \n", paste0(req_col_diff, collapse = ", "), "\n")

  # Check that all input columns are valid column names
  columns <- c(unlist(sapply(req_tbls, coleo_get_name_table_column)),
               "site_code") # site_code needs to be manually added
  possible_col_diff <- setdiff(names(data), columns)

  if(length(possible_col_diff) != 0) warning("Vérifiez que les bons noms de colonnes sont utilisés et que les colonnes superflues sont retirées. Les colonnes valides peuvent être : \n", paste0(columns, collapse = ", "), "\n\nLes colonnes au nom invalide sont : \n", paste0(possible_col_diff, collapse = ", "), "\n")

  # Check that input column types are valid
  tbl <- coleo_get_name_table()
  dat_names <- names(data)

  ## Check that all values within each column is of the right class
  valid_cols <- dat_names[dat_names %in% columns]
  class_of_col <- sapply(valid_cols, function(x) {
    class_of_col_values <- sapply(data[,x], tbl$required_class[[which(tbl$input_column==x)]])
    all(class_of_col_values)
  })

  ## Are all input columns of the right class?
  erroneous_cols <- dat_names[!class_of_col]
  if(!all(class_of_col)) warning("Vérifiez la classe des colonnes. Ces colonnes sont problématiques : \n", paste0(erroneous_cols, collapse = ", "), "\n")

  # Check that the range of values contained within input columns are valid
  tbl_with_legal_values <- subset(tbl, !is.na(legal_values))
  cols_to_check <- intersect(dat_names, tbl_with_legal_values$input_column)

  valid_col_values <- sapply(cols_to_check, function(x) {
    legal_vals <- tbl$legal_values[which(tbl$input_column==x)][[1]]
    all(sapply(unique(data[,x]), function(x) x %in% legal_vals))
  })

  invalid_cols <- which(tbl$input_column %in% names(valid_col_values)[!valid_col_values])
  cols_valid_values <- tbl$legal_values[invalid_cols]
  names(cols_valid_values) <- tbl$input_column[invalid_cols]
  col_names <- tbl$input_column[invalid_cols]

  if(!all(valid_col_values)) warning("Vérifiez les valeurs contenues dans les colonnes. Ces colonnes contiennent des valeurs invalides : \n", paste0(col_names, collapse = ", "), "\n\nLes valeurs possibles pour ces colonnes sont : \n", paste0(col_names, ": ", cols_valid_values, collapse = "\n"), "n")



  # Check that the format of the input column date is valid

  ## Identify columns containing dates
  #strsplit(tbl$input_column, "\date", fixed = TRUE)
  cols_date_name <- tbl$input_column[grepl("date", tbl$input_column, fixed = TRUE)]
  cols_date <- cols_date_name[cols_date_name %in% dat_names]


  if(length(cols_date) > 0) {

    ## Check that the year contains 4 digits, the month 2, and the day 2
    cols_ndigits <- sapply(cols_date, function(x) {
      split <- strsplit(unlist(data[,x]), "-", fixed = TRUE)
      date_ndigits <- sapply(split, nchar)
      all(date_ndigits[1,] == 4, date_ndigits[2,] == 2, date_ndigits[3,] == 2)
    })
    is_ndigits_valid <- all(cols_ndigits)

    if(!is_ndigits_valid) warning("Vérifiez le format des valeurs de dates. Les dates doivent etre du format YYYY-MM-DD.\n")
  }

## Check that the values are within a decent range
if(length(cols_date) > 0) {
  ### Year
  range_year <- sapply(data[cols_date], function(x) {
    split <- strsplit(unlist(x), "-", fixed = TRUE)
    range(as.numeric(sapply(split, `[[`, 1)))
  }) |>
    range()
  ### Month
  range_month <- sapply(data[cols_date], function(x) {
    split <- strsplit(unlist(x), "-", fixed = TRUE)
    range(as.numeric(sapply(split, `[[`, 2)))
  }) |>
    range()
  ### Day
  range_day <- sapply(data[cols_date], function(x) {
    split <- strsplit(unlist(x), "-", fixed = TRUE)
    range(as.numeric(sapply(split, `[[`, 3)))
  }) |>
    range()

  message(paste0("Dernière étape ! \nVérifiez que l'intervalle des dates injectées correspond aux attentes. Les valeurs de dates des colonnes ", paste0(cols_date, collapse = ",")," se trouvent dans l'intervalle de l'année ", range_year[1], " à ", range_year[2], " du mois ", range_month[1], " à ", range_month[2], " et du jour ", range_day[1], " à ", range_day[2], "\n\nSi les dates sont bonnes et qu'aucun autre message n'apparait, vous pouvez procéder à l'injection des données"))
}

# Check that the format of the input column containing time is valid

}
