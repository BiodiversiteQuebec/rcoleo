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
                          msg = "Vérifiez qu'une colonne contient le code du site et que le nom de colone correspond à site_code")

  # Check that the imported data has all of the required columns
  req_tbls <- coleo_return_required_tables(campaign_type)
  req_columns <- c(unlist(sapply(req_tbls, coleo_get_required_name_table)),
                   "site_code") # site_code needs to be manually added
  req_col_diff <- setdiff(req_columns, names(data))

  assertthat::assert_that(length(req_col_diff) == 0,
                          msg = paste0("Vérifiez que les bons noms de colonnes sont utilisés et que toutes les colonnes requises sont présentes. Les colonnes requises sont : \n", paste0(req_columns, collapse = ", "), "\n\nLes colonnes absentes sont : \n", paste0(req_col_diff, collapse = ", ")))

  # Check that all input columns are valid column names
  columns <- c(unlist(sapply(req_tbls, coleo_get_name_table_column)),
               "site_code") # site_code needs to be manually added
  possible_col_diff <- setdiff(names(data), columns)

  assertthat::assert_that(length(possible_col_diff) == 0,
                          msg = paste0("Vérifiez que les bons noms de colonnes sont utilisés et que les colonnes superflues sont retirées. Les colonnes valides peuvent être : \n", paste0(columns, collapse = ", "), "\n\nLes colonnes au nom invalide sont : \n", paste0(possible_col_diff, collapse = ", ")))

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
                          msg = paste0("Vérifiez la classe des colonnes. Ces colonnes sont problématiques : \n", paste0(erroneous_cols, collapse = ", ")))

  # Check that the range of values contained within input columns are valid
  tbl_with_legal_values <- subset(tbl, !is.na(legal_values))
  cols_to_check <- intersect(dat_names, tbl_with_legal_values$input_column)

  valid_col_values <- sapply(cols_to_check, function(x) {
    legal_vals <- tbl$legal_values[which(tbl$input_column==x)][[1]]
    all(unique(data[,x]) %in% legal_vals)
    })

  invalid_cols <- which(tbl$input_column %in% names(valid_col_values)[!valid_col_values])
  cols_valid_values <- tbl$legal_values[invalid_cols]
  names(cols_valid_values) <- tbl$input_column[invalid_cols]
  col_names <- tbl$input_column[invalid_cols]

  assertthat::assert_that(all(valid_col_values),
                          msg = paste0("Vérifiez les valeurs contenues dans les colonnes. Ces colonnes contiennent des valeurs invalides : \n", paste0(col_names, collapse = ", "), "\n\nLes valeurs possibles pour ces colonnes sont : \n", paste0(col_names, ": ", cols_valid_values, collapse = "\n")))





  # Check that the format of the input column date is valid

  ## Identify columns containing dates
  #strsplit(tbl$input_column, "\date", fixed = TRUE)
  cols_date_name <- tbl$input_column[grepl("date", tbl$input_column, fixed = TRUE)]
  cols_date <- cols_date_name[cols_date_name %in% dat_names]

  if(length(cols_date) > 0) {

    ## Check that the year contains 4 digits, the month 2, and the day 2
    split <- strsplit(data[,cols_date], "-", fixed = TRUE)
    date_ndigits <- sapply(split, nchar)
    is_ndigits_valid <- all(date_ndigits[1,] == 4, date_ndigits[2,] == 2, date_ndigits[3,] == 2)

    assertthat::assert_that(is_ndigits_valid,
                            msg = paste0("Vérifiez le format des valeurs de dates. Les dates doivent etre du format YYYY-MM-DD." ))


    ## Check that the values are within decent a range
    range_year<- range(as.numeric(sapply(split, `[[`, 1)))
    range_month <- range(as.numeric(sapply(split, `[[`, 2)))
    range_day <- range(as.numeric(sapply(split, `[[`, 3)))

    message(paste0("Vérifiez que l'intervalle des dates injectées correspond aux attentes. Les valeurs de dates se trouvent dans l'intervalle de l'année ", range_year[1], " à ", range_year[2], " du mois ", range_month[1], " à ", range_month[2], " et du jour ", range_day[1], " à ", range_day[2]))
  }

  # Check that the format of the input column containing time is valid

}
