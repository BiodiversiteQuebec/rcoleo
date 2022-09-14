#' Valide la forme du jeu de données à injecter
#'
#' @param data jeu de données à valider
#'
#' @return
#' @export
#'
coleo_validate <- function(data) {

  #------------------------------------------------------------------------
  # Increase default warning message length
  #------------------------------------------------------------------------
  options(warning.length = 2000L)


  #------------------------------------------------------------------------
  # Check that there is a campaign type column and that it contains a unique value
  #------------------------------------------------------------------------
  # Pursue only if there is a campaign type column
  if(!assertthat::has_name(data, "campaigns_type")) stop("V\U00E9rifiez qu'une colonne contient le type de campagne et que son nom de colonne correspond à campaigns_type \nLe type de campagne est nécessaire pour les prochaines étapes de validation.\n\n")

  # Pursue only if there is a campaign type value
  campaign_type <- unique(data$campaigns_type)
  campaigns <- coleo_return_valid_campaigns()
  if(!(length(campaign_type) == 1 && campaign_type %in% campaigns)) stop("V\U00E9rifiez que toutes les valeurs de la colonne campaigns_type sont identiques et que la valeur est un type de campagne valide. \nLe type de campagne est nécessaire pour les prochaines étapes de validation.\n\n")


  #------------------------------------------------------------------------
  # Df columns, true column names, class and accepted values
  #------------------------------------------------------------------------
  # Helper fct that converts col names to corresponding table_field col names
  # - data_names: column names of the dataFile
  # - col_names: column names for the campaign type
  to_true_names <- function(data_names, col_names) {
    for(nm in seq_along(data_names)) {
      if(!data_names[nm] %in% col_names) {
        # Extra columns must remain
        if (grepl("_extra", data_names[nm])) next
        ## Some columns may have modified names
        ## eg. lures_lure_1
        mod_col_nm <- sapply(col_names,
          function(x) grepl(x, data_names[nm])
        )
        if (any(mod_col_nm)) {
          true_col_nm <- names(which(mod_col_nm))
          ## Select the longuest name - In case there is multiple matching
          true_col_nm <- true_col_nm[which.max(nchar(true_col_nm))]
          ## Change col name
          data_names[nm] <- true_col_nm
        }
      }
    }
    return(data_names)
  }

  tbl <- coleo_return_cols(campaign_type)
  columns <- tbl$noms_de_colonnes
  dat_names <- names(data)
  true_nms <- to_true_names(dat_names, columns)


  #------------------------------------------------------------------------
  # Check that the imported data has all of the required columns
  #------------------------------------------------------------------------
  # Compare required column names to present columns ----------------------
  req_columns <- coleo_return_cols(campaign_type, required.columns = TRUE)$noms_de_colonnes
  req_col_diff <- setdiff(req_columns, true_nms)
  # Return warning if there's a mismatch ----------------------------------
  if(length(req_col_diff) != 0) warning("--------------------------------------------------\nV\U00E9rifiez que les bons noms de colonnes sont utilis\U00E9s et que toutes les colonnes requises sont pr\U00E9sentes.\n", "\n\nLes colonnes absentes sont : \n", paste0(req_col_diff, collapse = ", "), "\n\n")


  #------------------------------------------------------------------------
  # Check that all input columns are valid column names
  #------------------------------------------------------------------------
  # Accept colnames that contains extra specifiers in it ------------------
  # - eg. lures_lure_1 = lures_lure ---------------------------------------
  # Test for invalid column names
  possible_col_diff <- setdiff(true_nms, columns)
  # Accept colnames that contains "extra" in it - extra columns -----------
  which_extra <- grepl("extra", possible_col_diff)
  possible_col_diff <- possible_col_diff[!which_extra]
  # Accept media_name column ----------------------------------------------
  possible_col_diff <- which(possible_col_diff != "media_name")

  if(length(possible_col_diff) != 0) warning("--------------------------------------------------\nV\U00E9rifiez que les bons noms de colonnes sont utilis\U00E9s et que les colonnes superflues sont", paste0(" retir\U00E9", "es"), ".\n", "\n\nLes colonnes au nom invalide sont : \n", paste0(possible_col_diff, collapse = ", "), "\n\n")


  #------------------------------------------------------------------------
  # Check that all NAs are NAs
  #------------------------------------------------------------------------
  is_char_na <- any(data == "NA", na.rm = TRUE) |> suppressWarnings()
  if(is_char_na) {
    data[data == "NA"] <- NA
    warning("--------------------------------------------------\nV\U00E9rifiez les champs sans valeurs. Ils devraient \U00E2tre NA, mais certains contiennent la valeur de 'NA' en charactères.\n\n")
  }
  

  #------------------------------------------------------------------------
  # Check that input column types are valid
  # - Test true_nms as it captures the true column name and its associated class
  #------------------------------------------------------------------------
  # Check that all values within each column is of the right class --------
  # - Extra columns remain as characters
  cols_to_valid <- dat_names[true_nms %in% columns]
  cols_to_valid <- cols_to_valid[!grepl("_extra", cols_to_valid)]

  class_of_col <- sapply(cols_to_valid, function(x) {
    class_of_col_values <- sapply(data[,x], function(col_class) {
      true_col_nm <- true_nms[which(dat_names == x)]
      expected_class <- tbl$classe[[which(tbl$noms_de_colonnes == true_col_nm)]]
      # If an integer, check that it is a full number
      if (expected_class == "integer") {
        ifelse(is.numeric(col_class), col_class %% 1 == 0, FALSE) 
      } else if (expected_class == "list") {
        ## Check if column is a list
        if (inherits(data[,x][[1]], "list")) { # Workaround issues for items within lists being characters - Only asses class at the column scale
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
    all(class_of_col_values, na.rm = TRUE)
  })

  # Are all input columns of the right class? -----------------------------
  erroneous_cols <- dat_names[!class_of_col]
  if(!all(class_of_col)) warning("--------------------------------------------------\nV\U00E9rifiez la classe des colonnes. Ces colonnes sont probl\U00E9matiques : \n", paste0(erroneous_cols, collapse = ", "), "\n\n")


  #------------------------------------------------------------------------
  # Check that all sites exists in coleo
  #------------------------------------------------------------------------
  existing_sites <- coleo_request_general(endpoint = "sites", response_as_df = TRUE)
  
  are_sites_exists <- all(unique(data$sites_site_code) %in% existing_sites$site_code)
  # Missing sites ---------------------------------------------------------
  sites_x <- which(!unique(data$sites_site_code) %in% existing_sites$site_code)

  if(!are_sites_exists) warning("--------------------------------------------------\nV\U00E9rifiez les sites ", dput(unique(data$sites_site_code)[sites_x]), " de la colonne sites_site_code ou injectez ces sites dans la table sites de coleo. Cette colonne contient des sites qui n'existent pas dans coleo.\n\n")


  #------------------------------------------------------------------------
  # Check that all campaigns without observations have all fields of
  # taxonomic level equal to or inferior to the observation are NA in
  # "empty" campaigns
  # - Test only for campaigns with obs_species
  #------------------------------------------------------------------------
  if ("obs_species_taxa_name" %in% dat_names) {
    # Identify columns that need and need not to be NA if campaigns are empty
    no_na_tbls <- c("cells", "sites", "campaigns", "efforts", "environments", "devices", "lures", "traps", "landmarks", "samples", "thermographs")
    which_no_na_tbls <- sapply(no_na_tbls, function(x) grepl(x, dat_names) |> which()) |> unlist() |> unique()
    na_cols <- dat_names[-which_no_na_tbls]
    # Loop through rows to validate that observations related fields are NA if no observations
    row_not_empty <- c()
    no_obs <- 0
    for (row in 1:nrow(data)) {
      is_obs_na <- data$obs_species_taxa_name[row] |> is.na()
      ## If no observation, then all fields of taxonomic level equal or lower to the observation need to be NA
      if(is_obs_na) {
        no_obs <- no_obs + 1
        is_row_na <- data[row,na_cols] |> is.na() |> all()
        # Save results --------------------------------------------------------
        if(!is_row_na) row_not_empty <- c(row_not_empty, row)
      }
    }

    if(!is.null(row_not_empty)) warning(paste0("--------------------------------------------------\nV\U00E9rifiez les lignes sans observation. Les champs qui d\U00E9crivent le jeu de données, la localisation de l\'\U00E9chantillonnage et les détails sur la campagne d'\U00E9chantillonnage doivent être remplis, mais tous les autres champs qui décrivent l\'observation doivent être laissés vide.\n\n"))
  }


  #------------------------------------------------------------------------
  # Check that (and only) ADNe campaigns observations made at the station level
  # have landmarks
  #------------------------------------------------------------------------
  if ("observations_extra_value_1" %in% dat_names) {

    # Check that all lake observations do not have landmarks
    lac_rows <- which(data$observations_extra_value_1 == "lac")

    if (length(lac_rows) > 0) {
      is_true <- all(is.na(data[lac_rows, c("landmarks_type", "landmarks_lat", "landmarks_lon")]))

      if(!is_true) warning(paste0("--------------------------------------------------\nV\U00E9rifiez les observations faites à l'échelle du lac. Les colonnes landmarks_type, landmarks_lat et landmarks_lon doivent être laissées vide pour ces lignes\n\n"))
    }

    # Check that all station observations have landmarks
    station_rows <- which(data$observations_extra_value_1 == "station")
    
    if(length(station_rows) > 0) {
      is_true <- all(!is.na(data[station_rows, c("landmarks_type", "landmarks_lat", "landmarks_lon")]))
      
      if(!is_true) warning(paste0("--------------------------------------------------\nV\U00E9rifiez les observations qui se rattachent aux stations. Les colonnes landmarks_type, landmarks_lat et landmarks_lon doivent être remplies pour ces lignes\n\n"))
    }
  }

  #------------------------------------------------------------------------
  # Check that ADNe campaigns at the lake scale have corrected sequence count
  #------------------------------------------------------------------------


  #------------------------------------------------------------------------
  # Check that the range of values contained within input columns are valid
  #------------------------------------------------------------------------
  tbl_with_legal_values <- subset(tbl, !is.na(valeurs_acceptees))
  cols_to_check <- intersect(true_nms, tbl_with_legal_values$noms_de_colonnes)

  valid_col_values <- sapply(cols_to_check, function(x) {
    legal_vals <- tbl$valeurs_acceptees[which(tbl$noms_de_colonnes == x)][[1]]
    i <- which(true_nms == x)
    ## Also accept NAs
    legal_vals <- c(legal_vals, NA)
    all(sapply(unique(data[,i]), function(x) x %in% legal_vals))
  })

  invalid_cols <- which(tbl$noms_de_colonnes %in% names(valid_col_values)[!valid_col_values])
  cols_valid_values <- tbl$valeurs_acceptees[invalid_cols]
  names(cols_valid_values) <- tbl$noms_de_colonnes[invalid_cols]
  col_names <- tbl$noms_de_colonnes[invalid_cols]

  if(!all(valid_col_values)) warning("--------------------------------------------------\nV\U00E9rifiez les valeurs contenues dans les colonnes. Ces colonnes contiennent des valeurs invalides : \n", paste0(col_names, collapse = ", "), "\n\nLes valeurs possibles pour ces colonnes sont : \n", paste0(col_names, ": ", cols_valid_values, collapse = "\n"), "\n\n")


  #------------------------------------------------------------------------
  # Check that complexes of species are correctly formated
  #------------------------------------------------------------------------
  is_complexes <- grepl("|", data$obs_species_taxa_name) |> which()
  if(!length(is_complexes) == 0) {
    cplx_valid <-sapply(data$obs_species_taxa_name[is_complexes], function(cplx) {
      cplx_split <- strsplit(cplx, "|", fixed = TRUE) |> 
        unlist() |>
        stringr::str_trim()
      cplx_formated <- paste(cplx_split, collapse = " | ")
      identical(cplx, cplx_formated)
    })

    if(!all(cplx_valid)) warning("--------------------------------------------------\nV\U00E9rifiez le format des complexes d'espèces : certain complexes sont incorrectement formatés. Les taxons composants le complexe doivent être séparés par une barre verticale flanquée d'un espace de chaque côté \" | \". Exemple : \"Acer saccharum | Acer negundo\"\n\n")

  }


  #------------------------------------------------------------------------
  # Check that variable field of obs_species table is valid
  #------------------------------------------------------------------------
  if("obs_species_variable" %in% dat_names) {
    var <- unique(data$obs_species_variable)
    possible_vars <- coleo_get_attributes_table(column = "variable")
    possible_vars <- c(possible_vars, NA) # Accept NAs in empty campaigns
    are_vars_valid <- all(var %in% possible_vars)

    if(!are_vars_valid) warning("--------------------------------------------------\nV\U00E9rifiez les valeurs ", dput(var[!var %in% possible_vars]), " de la colonne obs_species_variable ou injectez ces valeurs dans la table attributes. Cette colonne contient une valeur qui n'est pas une valeur de la table attributes\n\n")
  }


  #------------------------------------------------------------------------
  # Check that the format of the input column containing time is valid
  #------------------------------------------------------------------------
  # Function to test digits number ----------------------------------------
  time_digits <- function(time_ndigits) {
    all(time_ndigits[1,] == 2, time_ndigits[2,] == 2, time_ndigits[3,] == 2)
  }
  possibly_time_digits <- purrr::possibly(time_digits, otherwise = FALSE)
  # Identify columns containing time --------------------------------------
    cols_time_name <- tbl$noms_de_colonnes[
      grepl("time", tbl$noms_de_colonnes, fixed = TRUE)]
    cols_time <- cols_time_name[cols_time_name %in% dat_names]

  if (length(cols_time) > 0) {

    # Check time format (HH:MM:SS) ----------------------------------------
    na_in_time <- c(FALSE)
    cols_format <- sapply(cols_time, function(x) {
      split <- strsplit(unlist(data[,x]), ":", fixed = TRUE)
      na <- is.na(split)
      # Accept NAs in certain cols when campaign is empty
      na_in_time <- ifelse(x %in% na_cols, c(na_in_time, FALSE), c(na_in_time, any(na)))
      split <- split[!na]
      time_ndigits <- sapply(split, nchar)
      possibly_time_digits(time_ndigits)
    })
    is_time_format <- all(cols_format)
    is_time_na <- any(na_in_time)

    if(!is_time_format | is_time_na) warning("--------------------------------------------------\nV\U00E9rifiez le format des valeurs d'heure. L'heure doit etre du format \"HH:MM:SS\".\n\n")
  }


  #------------------------------------------------------------------------
  # Check that the format of the input column date is valid
  #------------------------------------------------------------------------
  # Identify columns containing dates -------------------------------------
  cols_date_name <- tbl$noms_de_colonnes[
    grepl("_at", tbl$noms_de_colonnes, fixed = TRUE) |
      grepl("_date", tbl$noms_de_colonnes, fixed = TRUE)]
  cols_date <- cols_date_name[cols_date_name %in% dat_names]


  if(length(cols_date) > 0) {

    # Check that the year contains 4 digits, the month 2, and the day 2 ---
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

    if(!is_ndigits_valid | is_na) warning("--------------------------------------------------\nV\U00E9rifiez le format des valeurs de dates. Les dates doivent \U00EAtre du format YYYY-MM-DD.\n\n")
  }

  # Check that the values are within a decent range -----------------------
  if(length(cols_date) > 0) {
    # Year
    range_year <- sapply(data[cols_date], function(x) {
      split <- strsplit(unlist(x), "-", fixed = TRUE)
      split <- split[!is.na(split)]
      range(as.numeric(sapply(split, `[[`, 1)))
    }) |>
      range()
    # Month
    range_month <- sapply(data[cols_date], function(x) {
      split <- strsplit(unlist(x), "-", fixed = TRUE)
      split <- split[!is.na(split)]
      range(as.numeric(sapply(split, `[[`, 2)))
    }) |>
      range()
    # Day
    range_day <- sapply(data[cols_date], function(x) {
      split <- strsplit(unlist(x), "-", fixed = TRUE)
      split <- split[!is.na(split)]
      range(as.numeric(sapply(split, `[[`, 3)))
    }) |>
      range()

    message(paste0("==================================================\nValidation finale ! \n",
    if ("obs_species_taxa_name" %in% dat_names) paste0("- V\U00E9rifiez les lignes qui représentent des campagnes vides. Il y a ", no_obs, " lignes sans observations.\n"),
    "- V\U00E9rifiez que l'intervalle des dates", paste0(" inject\U00E9", "es "), "correspond aux attentes. Les valeurs de dates des colonnes ", paste0(cols_date, collapse = ", "), " se trouvent dans l'intervalle de", paste0(" l'ann\U00E9", "e "), range_year[1], " \U00E0 ", range_year[2], " du mois ", range_month[1], " \U00E0 ", range_month[2], " et du jour ", range_day[1], "  \U00E0 ", range_day[2], ".\n
    - Si les", paste0(" donn\U00E9", "es"), " sont bonnes et qu'aucun autre message n'apparait, vous pouvez", paste0(" proc\U00E9", "der"), " à l'injection des", paste0(" donn\U00E9", "es.")))
  }
}
