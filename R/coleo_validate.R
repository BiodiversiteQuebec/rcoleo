#' Valide la forme du jeu de données à injecter
#'
#' @param data jeu de données à valider
#'
#' @return
#' @export
#'
coleo_validate <- function(data) {

  # Increase default warning message length
  options(warning.length = 2000L)

  # Check that there is a campaign type column and that it contains a unique value
  if(!assertthat::has_name(data, "campaigns_type")) stop("Vérifiez qu'une colonne contient le type de campagne et que son nom de colonne correspond à campaigns_type \nLe type de campagne est nécessaire pour les prochaines étapes de validation.\n\n")

  campaign_type <- unique(data$campaigns_type)
  campaigns <- coleo_return_valid_campaigns()
  if(!(length(campaign_type) == 1 && campaign_type %in% campaigns)) stop("Vérifiez que toutes les valeurs de la colonne campaigns_type sont identiques et que la valeur est un type de campagne valide. \nLe type de campagne est nécessaire pour les prochaines étapes de validation.\n\n")

  # Check that the imported data has all of the required columns
  ## compare required column names to present columns
  req_columns <- coleo_return_required_cols(campaign_type)$noms_de_colonnes
  req_col_diff <- setdiff(req_columns, names(data))
  ## Return warning if there's a mismatch
  if(length(req_col_diff) != 0) warning("Vérifiez que les bons noms de colonnes sont utilisés et que toutes les colonnes requises sont présentes. Les colonnes requises sont : \n", paste0(req_columns, collapse = ", "), "\n\nLes colonnes absentes sont : \n", paste0(req_col_diff, collapse = ", "), "\n\n")

  # Check that all input columns are valid column names
  columns <- coleo_return_cols(campaign_type)$noms_de_colonnes
  possible_col_diff <- setdiff(names(data), columns)

  ## Accept colnames that contains "extra" in it - extra columns
  which_extra <- grepl("extra", possible_col_diff)
  possible_col_diff <- possible_col_diff[!which_extra]

  if(length(possible_col_diff) != 0) warning("Vérifiez que les bons noms de colonnes sont utilisés et que les colonnes superflues sont retirées. Les colonnes valides peuvent être : \n", paste0(columns, collapse = ", "), "\n\nLes colonnes au nom invalide sont : \n", paste0(possible_col_diff, collapse = ", "), "\n\n")

  # Check that input column types are valid
  dat_names <- names(data)

  ## Check that all values within each column is of the right class
  valid_cols <- dat_names[dat_names %in% columns]
  tbl <- coleo_return_cols(campaign_type)
  class_of_col <- sapply(valid_cols, function(x) {
    class_of_col_values <- sapply(data[,x], function(col_class) {
      expected_class <- tbl$classe[[which(tbl$noms_de_colonnes == x)]]
      ## If an integer, check that it is a full number
      if (expected_class == "integer") {
        col_class %% 1 == 0
      } else if (expected_class == "list") {
        ## Check if column is a list
        if (class(data[,x][[1]]) == "list") { # Workaround issues for items within lists being characters - Only asses class at the column scale
          TRUE
        } else {
          ## If not a list, check if there is commas, and if there is none, accept characters
          is_there_comas <- sapply(col_class, function(value) grepl(",", value)) |> any()
          if (is_there_comas) {
            FALSE
          } else {
            class(data[,x][[1]]) == "character"
          }
        }
      } else {
        class(col_class) == expected_class
      }
    })
    all(class_of_col_values)
  })

  ## Are all input columns of the right class?
  erroneous_cols <- dat_names[!class_of_col]
  if(!all(class_of_col)) warning("Vérifiez la classe des colonnes. Ces colonnes sont problématiques : \n", paste0(erroneous_cols, collapse = ", "), "\n\n")

  # Check that the range of values contained within input columns are valid
  tbl_with_legal_values <- subset(tbl, !is.na(valeurs_acceptees))
  cols_to_check <- intersect(dat_names, tbl_with_legal_values$noms_de_colonnes)

  valid_col_values <- sapply(cols_to_check, function(x) {
    legal_vals <- tbl$valeurs_acceptees[which(tbl$noms_de_colonnes==x)][[1]]
    all(sapply(unique(data[,x]), function(x) x %in% legal_vals))
  })

  invalid_cols <- which(tbl$noms_de_colonnes %in% names(valid_col_values)[!valid_col_values])
  cols_valid_values <- tbl$valeurs_acceptees[invalid_cols]
  names(cols_valid_values) <- tbl$noms_de_colonnes[invalid_cols]
  col_names <- tbl$noms_de_colonnes[invalid_cols]

  if(!all(valid_col_values)) warning("Vérifiez les valeurs contenues dans les colonnes. Ces colonnes contiennent des valeurs invalides : \n", paste0(col_names, collapse = ", "), "\n\nLes valeurs possibles pour ces colonnes sont : \n", paste0(col_names, ": ", cols_valid_values, collapse = "\n"), "\n\n")

  # Check that variable field of obs_species table is valid
  if("obs_species_variable" %in% dat_names) {
    var <- unique(data$obs_species_variable)
    possible_vars <- coleo_get_attributes_table(column = "variable")
    are_vars_valid <- all(var %in% possible_vars)

    if(!are_vars_valid) warning("Vérifiez les valeurs ", dput(var[!var %in% possible_vars]), " de la colonne obs_species_variable ou injectez ces valeurs dans la table attributes. Cette colonne contient une valeur qui n'est pas une valeur de la table attributes\n\n")
  }

  # Check that the format of the input column containing time is valid
  ## Function to test digits number
  time_digits <- function(time_ndigits) {
    all(time_ndigits[1,] == 2, time_ndigits[2,] == 2, time_ndigits[3,] == 2)
  }
  possibly_time_digits <- purrr::possibly(time_digits, otherwise = FALSE)
  ## Identify columns containing time
    cols_time_name <- tbl$noms_de_colonnes[
      grepl("time", tbl$noms_de_colonnes, fixed = TRUE)]
    cols_time <- cols_time_name[cols_time_name %in% dat_names]

  if (length(cols_time) > 0) {

    ## Check time format (HH:MM:SS)
    na_in_time <- c(FALSE)
    cols_format <- sapply(cols_time, function(x) {
      split <- strsplit(unlist(data[,x]), ":", fixed = TRUE)
      na <- is.na(split)
      na_in_time <- c(na_in_time, any(na))
      split <- split[!na]
      time_ndigits <- sapply(split, nchar)
      possibly_time_digits(time_ndigits)
    })
    is_time_format <- all(cols_format)
    is_time_na <- any(na_in_time)

    if(!is_time_format | is_time_na) warning("Vérifiez le format des valeurs d'heure. L'heure doit etre du format \"HH:MM:SS\".\n\n")
  }

  # Check that the format of the input column date is valid

  ## Identify columns containing dates
  cols_date_name <- tbl$noms_de_colonnes[
    grepl("_at", tbl$noms_de_colonnes, fixed = TRUE) |
      grepl("_date", tbl$noms_de_colonnes, fixed = TRUE)]
  cols_date <- cols_date_name[cols_date_name %in% dat_names]


  if(length(cols_date) > 0) {

    ## Check that the year contains 4 digits, the month 2, and the day 2
    na_in_dates <- c(FALSE)
    cols_ndigits <- sapply(cols_date, function(x) {
      split <- strsplit(unlist(data[,x]), "-", fixed = TRUE)
      na <- is.na(split)
      na_in_dates <- c(na_in_dates, any(na))
      split <- split[!na]
      date_ndigits <- sapply(split, nchar)
      all(date_ndigits[1,] == 4, date_ndigits[2,] == 2, date_ndigits[3,] == 2)
    })
    is_ndigits_valid <- all(cols_ndigits)
    is_na <- any(na_in_dates)

    if(!is_ndigits_valid | is_na) warning("Vérifiez le format des valeurs de dates. Les dates doivent etre du format YYYY-MM-DD.\n\n")
  }

  ## Check that the values are within a decent range
  if(length(cols_date) > 0) {
    ### Year
    range_year <- sapply(data[cols_date], function(x) {
      split <- strsplit(unlist(x), "-", fixed = TRUE)
      split <- split[!is.na(split)]
      range(as.numeric(sapply(split, `[[`, 1)))
    }) |>
      range()
    ### Month
    range_month <- sapply(data[cols_date], function(x) {
      split <- strsplit(unlist(x), "-", fixed = TRUE)
      split <- split[!is.na(split)]
      range(as.numeric(sapply(split, `[[`, 2)))
    }) |>
      range()
    ### Day
    range_day <- sapply(data[cols_date], function(x) {
      split <- strsplit(unlist(x), "-", fixed = TRUE)
      split <- split[!is.na(split)]
      range(as.numeric(sapply(split, `[[`, 3)))
    }) |>
      range()

    message(paste0("Dernière étape ! \nVérifiez que l'intervalle des dates injectées correspond aux attentes. Les valeurs de dates des colonnes ", paste0(cols_date, collapse = ", ")," se trouvent dans l'intervalle de l'année ", range_year[1], " à ", range_year[2], " du mois ", range_month[1], " à ", range_month[2], " et du jour ", range_day[1], " à ", range_day[2], "\n\nSi les dates sont bonnes et qu'aucun autre message n'apparait, vous pouvez procéder à l'injection des données\n\n==================================================\n"))
  }
}
