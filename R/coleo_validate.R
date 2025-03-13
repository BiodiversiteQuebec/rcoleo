#' Valide la forme du jeu de données à injecter
#'
#' @param data jeu de données à valider
#' @param media_path chemin vers le dossier contenant les médias à injecter
#'
#' @return Un message sur la validité du jeu de données.
#'
#' @export
#'
coleo_validate <- function(data, media_path = NULL) {

  #------------------------------------------------------------------------
  # Increase default warning message length
  #------------------------------------------------------------------------
  options(warning.length = 2000L)


  #------------------------------------------------------------------------
  # Check that there is a campaign type column and that it contains a unique value
  #------------------------------------------------------------------------
  campaign_type <- coleo_return_campaign_type(data)
  if (is.null(campaign_type)) stop("V\u00E9rifiez qu'une colonne contient le type d'inventaire (campaigns_type ou remote_sensing_indicators_name) et que son nom de colonne correspond \u00e0 campaigns_type \nLe type de campagne est n\u00E9cessaire pour les prochaines \u00E9tapes de validation.\n\n")
  if (length(campaign_type) > 1) stop("V\u00E9rifiez que toutes les valeurs de la colonne campaigns_type (ou remote_sensing_indicators_name) sont identiques et que la valeur est un type de campagne valide. \nLe type de campagne est n\u00E9cessaire pour les prochaines \u00E9tapes de validation.\n\n")
  if (length(campaign_type) == 0) stop("V\u00E9rifiez qu'une colonne contient le type d'inventaire (campaigns_type ou remote_sensing_indicators_name) et que son nom de colonne correspond \u00e0 campaigns_type \nLe type de campagne est n\u00E9cessaire pour les prochaines \u00E9tapes de validation.\n\n")
  
  campaigns <- coleo_return_valid_campaigns()
  if(!campaign_type %in% campaigns) stop("V\u00E9rifiez que toutes les valeurs de la colonne campaigns_type sont identiques et que la valeur est un type de campagne valide. \nLe type de campagne est n\u00E9cessaire pour les prochaines \u00E9tapes de validation.\n\n")


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
  req_columns <- tbl[tbl$colonne_requise==TRUE,]$noms_de_colonnes
  req_col_diff <- setdiff(req_columns, true_nms)
  # Remove media table names from required columns
  media_names <- grepl("media_", req_col_diff, fixed = TRUE)
  if (any(media_names)) req_col_diff <- req_col_diff[!media_names]
  # Remove observations_is_valid from required columns
  obs_is_valid <- grepl("observations_is_valid", req_col_diff, fixed = TRUE)
  if (any(obs_is_valid)) req_col_diff <- req_col_diff[!obs_is_valid]
  # Return warning if there's a mismatch ----------------------------------
  if(length(req_col_diff) != 0) warning("--------------------------------------------------\nV\u00E9rifiez que les bons noms de colonnes sont utilis\u00E9s et que toutes les colonnes requises sont pr\u00E9sentes. Les colonnes absentes sont : \n", paste0("- ", req_col_diff, collapse = "\n- "), "\n\n")


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
  possible_col_diff <- possible_col_diff[which(possible_col_diff != "media_name")]

  if(length(possible_col_diff) != 0) warning("--------------------------------------------------\nV\u00E9rifiez que les bons noms de colonnes sont utilis\u00E9s et que les colonnes superflues sont", paste0(" retir\u00E9", "es"), ".\n", "\n\nLes colonnes au nom invalide sont : \n", paste0(possible_col_diff, collapse = ", "), "\n\n")


  #------------------------------------------------------------------------
  # Check that all NAs are NAs
  #------------------------------------------------------------------------
  is_char_na <- any(apply(data, 2, function(x) sapply(x, function(y) any(!is.list(y)) && any(y == "NA"))), na.rm = TRUE) |> suppressWarnings()
  if(is_char_na) {
    data[data == "NA"] <- NA
    warning("--------------------------------------------------\nV\u00E9rifiez les champs sans valeurs. Ils devraient \u00E2tre NA, mais certains contiennent la valeur de 'NA' en charact\u00e8res.\n\n")
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
  if(!all(class_of_col)) warning("--------------------------------------------------\nV\u00E9rifiez la classe des colonnes. Ces colonnes sont ", paste0("probl","\U00E9","matiques "), " : \n", paste0(erroneous_cols, collapse = ", "), "\n\n")

  #------------------------------------------------------------------------
  # Check that all cells exists in coleo
  #------------------------------------------------------------------------
  if ("cells_cell_code" %in% dat_names) {
    existing_cells <- coleo_request_general(endpoint = "cells", response_as_df = TRUE, schema = 'public')
    
    are_cells_exists <- all(unique(data$cells_cell_code) %in% existing_cells$cell_code)
    # Missing cells ---------------------------------------------------------
    cells_x <- which(!unique(data$cells_cell_code) %in% existing_cells$cell_code)
    if (length(cells_x) > 10) {
      missing_cells <- paste0(paste0(unique(data$cells_cell_code)[cells_x[1:10]], collapse = ", "), " [...",length(cells_x)-10," tronquées]")
    } else {
      missing_cells <- paste0(unique(data$cells_cell_code)[cells_x], collapse = ", ")
    }

    if(!are_cells_exists) warning("--------------------------------------------------\n", paste0("V","\U00E9","rifiez")," les cellules ", missing_cells, " de la colonne cells_cell_code ou injectez ces cellules dans la table cells de coleo. Ces cellules n'existent pas dans coleo.\n\n")
  }

  #------------------------------------------------------------------------
  # Check that all sites exists in coleo
  #------------------------------------------------------------------------
  existing_sites <- coleo_request_general(endpoint = "sites", response_as_df = TRUE, schema = 'public')
  
  are_sites_exists <- all(unique(data$sites_site_code) %in% existing_sites$site_code)
  # Missing sites ---------------------------------------------------------
  sites_x <- which(!unique(data$sites_site_code) %in% existing_sites$site_code)
  if (length(sites_x) > 10) {
    missing_sites <- paste0(paste0(unique(data$sites_site_code)[sites_x[1:10]], collapse = ", ")," [...",length(sites_x)-10," tronqués]")
  } else {
    missing_sites <- paste0(unique(data$sites_site_code)[sites_x], collapse = ", ")
  }

  if(!are_sites_exists) warning("--------------------------------------------------\n", paste0("V","\U00E9","rifiez")," les sites ", missing_sites, " de la colonne sites_site_code ou injectez ces sites dans la table sites de coleo. Ces sites n'existent pas dans coleo.\n\n")


  #------------------------------------------------------------------------
  # Check that a media_path directory is provided
  #------------------------------------------------------------------------
  if ("media_name" %in% dat_names) {
    # Validate directory existence
    if (is.null(media_path)) warning("--------------------------------------------------\nLe chemin du répertoire contenant les ", paste0("m","\U00E9","dias"), " n'a pas ", paste0("\U00E9","t", "\U00E9", " pass", "\U00E9"), " \u00E0 l'argument mendia_path. La validation des valeurs de la colonne media_name est ignorée.\n\n")
  }
  
  #------------------------------------------------------------------------
  # Check that all media_name files exists in the provided directory
  #------------------------------------------------------------------------
  if (("media_name" %in% dat_names) & !is.null(media_path)) {
    # Validate directory existence
    if (!dir.exists(file.path(media_path))) warning("--------------------------------------------------\n", paste0("V","\U00E9","rifiez")," le ",paste0("d","\U00E9","pot "), " des ", paste0("m","\U00E9","dias"), ". ",  dput(media_path), " ", dput(media_path)," n'est pas un ",paste0("d","\U00E9","pot "), "valide.\n\n")
    
    photos <- dir(media_path)
    media_exists <- sapply(data$media_name[!is.na(data$obs_species_taxa_name)], function(file) {
      file %in% photos
      })

    if(any(!media_exists)) warning("--------------------------------------------------\n", paste0("V","\U00E9","rifiez")," les noms des ", paste0("m","\U00E9","dias"), " aux lignes ",  paste(which(!media_exists), collapse = ", "), " de la colonne media_name. Cette colonne contient des noms qui ne correspondent à aucun document dans le ", paste0("d","\U00E9","pot "), dput(media_path),".\n\n")
  }


  #------------------------------------------------------------------------
  # Check that campaigns, efforts and landmarks are not duplicated by comments
  #
  # - A comment added to a campaigns using the campaigns_notes column may
  #  cause the campaign to be duplicated if the comment is not the same
  #  for every row of the campaign.
  #------------------------------------------------------------------------
  # Campaigns with notes --------------------------------------------------
  if ("campaigns_notes" %in% dat_names) {
    # Get all unique campaigns
    camp_w_notes_cols <- c("sites_site_code", dat_names[grepl("campaigns_", dat_names)])
    camp_cols <- camp_w_notes_cols["campaigns_notes" != camp_w_notes_cols]
    camp_dupl <- identical(data[!duplicated(data[, camp_cols]), camp_cols], data[!duplicated(data[, camp_w_notes_cols]), camp_cols])
    if (!camp_dupl) {
      ## Get duplicated campaigns
      camp_dat <- data[!duplicated(data[, camp_w_notes_cols]), camp_cols]
      dupl_camp <- camp_dat$sites_site_code[duplicated(camp_dat)]
      ## Warning message
      warning("--------------------------------------------------\nV\u00E9rifiez les commentaires de campagnes (colonne campaigns_notes). Les commentaires de campagnes doivent normalement \u00EAtre saisis pour toutes les lignes de cette campagne. Ces campagnes sont \u00E0 risque d'être dupliqu\u00E9es :\n", paste(dupl_camp, collapse = ", "),".\n\n")
    }
  }

  # Efforts with notes ----------------------------------------------------
  if ("efforts_notes" %in% dat_names) {
    # Get all unique campaigns
    efforts_w_notes_cols <- dat_names[grepl("efforts_", dat_names) | grepl("campaigns_", dat_names) | grepl("sites_site_code", dat_names)]
    efforts_cols <- efforts_w_notes_cols["efforts_notes" != efforts_w_notes_cols]
    efforts_dupl <- identical(data[!duplicated(data[, efforts_cols]), efforts_cols], data[!duplicated(data[, efforts_w_notes_cols]), efforts_cols])

    if (!efforts_dupl) {
      ## Get duplicated efforts
      efforts_dat <- data[!duplicated(data[, efforts_w_notes_cols]), efforts_cols]
      dupl_efforts <- efforts_dat$sites_site_code[duplicated(efforts_dat)]
      ## Warning message
      warning("--------------------------------------------------\nV\u00E9rifiez les commentaires des efforts (colonne efforts_notes). Les commentaires des efforts doivent \u00EAtre saisis pour toutes les lignes de l'effort. Ces efforts sont dupliqu\u00E9s :\n", paste(dupl_efforts, collapse = ", "),".\n\n")
    }
  }

  # landmarks with notes --------------------------------------------------
  if ("landmarks_notes" %in% dat_names) {
    # Get all unique campaigns
    land_w_notes_cols <- dat_names[grepl("landmarks_", dat_names) | grepl("campaigns_", dat_names) | grepl("sites_site_code", dat_names)]
    land_cols <- land_w_notes_cols["landmarks_notes" != land_w_notes_cols]
    land_dupl <- identical(data[!duplicated(data[, land_cols]), land_cols], data[!duplicated(data[, land_w_notes_cols]), land_cols])

    if (!land_dupl) {
      ## Get duplicated landmarks
      land_dat <- data[!duplicated(data[, land_w_notes_cols]), land_cols]
      dupl_land <- land_dat$sites_site_code[duplicated(land_dat)]
      ## Warning message
      warning("--------------------------------------------------\nV\u00E9rifiez les commentaires des rep\u00E8res (colonne landmarks_notes). Les commentaires des rep\u00E8res doivent \u00EAtre saisis pour toutes les lignes du rep\u00E8re. Ces rep\u00E8res sont dupliqu\u00E9s :\n", paste(dupl_land, collapse = ", "),".\n\n")
    }
  }
  
  #------------------------------------------------------------------------
  # Check that all coordinates are within a valid range
  #
  # Québec bbox:  -79.76, 44.99, -57.10, 62.59
  #------------------------------------------------------------------------
  # Check that all coordinates are within valid range ---------------------
  proj_validation <- coleo_validate_coordinates_projection(data, dat_names)
  if (!is.na(proj_validation)) warning(proj_validation)


  # Check that all latitudes and longitudes  are within valid range -------
  Quebec_bbox <- c(-79.76, 44.99, -57.10, 62.59) # xmin, ymin, xmax, ymax
  bbox_validation <- coleo_validate_coordinates(data, dat_names, Quebec_bbox)

  if (!is.na(bbox_validation)) warning(bbox_validation)
  
  #------------------------------------------------------------------------
  # Check that azimut columns have values within 0 and 360
  #------------------------------------------------------------------------
  azimut_names <- grepl("azimut", dat_names)
  if (any(azimut_names)) {
    azimut_range <- unlist(data[,azimut_names]) |> range(na.rm = TRUE)

    if (azimut_range[2] > 360 | azimut_range[1] < 0) warning(paste0("--------------------------------------------------\nV\u00E9rifiez les valeurs d'azimut. Les valeurs doivent \u00EAtre entre 0 et 360.\n\n"))
  }


  #------------------------------------------------------------------------
  # Check that all campaigns without observations have all fields of
  # taxonomic level equal to or inferior to the observation are NA in
  # "empty" campaigns
  # - Test only for campaigns with obs_species
  #------------------------------------------------------------------------
  # Identify columns that need and need not to be NA if campaigns are empty
  no_na_tbls <- c("cells", "sites", "campaigns", "efforts", "environments", "devices", "lures", "traps", "landmarks", "samples", "thermographs")
  which_no_na_tbls <- sapply(no_na_tbls, function(x) grepl(x, dat_names) |> which()) |> unlist() |> unique()
  na_cols <- dat_names[-which_no_na_tbls]
  no_obs <- 0
  if ("obs_species_taxa_name" %in% dat_names) {
    # Loop through rows to validate that observations related fields are NA if no observations
    row_not_empty <- c()
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

    if(!is.null(row_not_empty)) warning(paste0("--------------------------------------------------\nV\u00E9rifiez les lignes sans observation. Les champs qui d\u00E9crivent le jeu de donn\u00E9es, la localisation de l\'\u00E9chantillonnage et les d\u00E9tails sur la campagne d'\u00E9chantillonnage doivent être remplis, mais tous les autres champs qui d\u00E9crivent l\'observation doivent être laiss\u00E9s vide.\n\n"))
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

      if(!is_true) warning(paste0("--------------------------------------------------\nV\u00E9rifiez les observations faites \u00e0 l'\u{00E9}chelle du lac. Les colonnes landmarks_type, landmarks_lat et landmarks_lon doivent être laiss\u00E9es vide pour ces lignes\n\n"))
    }

    # Check that all station observations have landmarks
    station_rows <- which(data$observations_extra_value_1 == "station")
    
    if(length(station_rows) > 0) {
      is_true <- all(!is.na(data[station_rows, c("landmarks_type", "landmarks_lat", "landmarks_lon")]))
      
      if(!is_true) warning(paste0("--------------------------------------------------\nV\u00E9rifiez les observations qui se rattachent aux stations. Les colonnes landmarks_type, landmarks_lat et landmarks_lon doivent être remplies pour ces lignes\n\n"))
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

  if(!all(valid_col_values)) warning("--------------------------------------------------\nV\u00E9rifiez les valeurs contenues dans les colonnes. Ces colonnes contiennent des valeurs invalides : \n", paste0(col_names, collapse = ", "), "\n\nLes valeurs possibles pour ces colonnes sont : \n", paste0(col_names, ": ", cols_valid_values, collapse = "\n"), "\n\n")

  #------------------------------------------------------------------------
  # Test for non-breaking spaces
  #------------------------------------------------------------------------
  notSpace <- apply(data, 1, function(x) {
    if(class(x) != "list"){
      grep("\u00A0", x) # non-breaking space
    }
  }) |>
      as.logical()

  if(any(notSpace)) warning("--------------------------------------------------\nV\u00E9rifiez la pr\u00E9sence d'espaces ins\u{00E9}cables aux lignes : ", paste0(which(notSpace), collapse = ", "), ", et remplacez les par des espaces standards\n\n")

  #------------------------------------------------------------------------
  # Check that complexes of species are correctly formated
  #------------------------------------------------------------------------
  if ("obs_species_taxa_name" %in% dat_names) {
    is_complexes <- grepl("|", data$obs_species_taxa_name) |> which()
    if(!length(is_complexes) == 0) {
      cplx_valid <-sapply(data$obs_species_taxa_name[is_complexes], function(cplx) {
        cplx_split <- strsplit(cplx, "|", fixed = TRUE) |> 
          unlist() |>
          stringr::str_trim()
        cplx_formated <- paste(cplx_split, collapse = " | ")
        identical(cplx, cplx_formated)
      })

      if(!all(cplx_valid)) warning("--------------------------------------------------\nV\u00E9rifiez le format des complexes d'esp\u00e8ces : certain complexes sont incorrectement format\u00E9s. Les taxons composants le complexe doivent être s\u00E9par\u00E9s par une barre verticale flanqu\u00E9e d'un espace de chaque côt\u00E9 \" | \". Exemple : \"Acer saccharum | Acer negundo\"\n\n")

    }
  }


  #------------------------------------------------------------------------
  # Check that variable field of obs_species table is valid
  #------------------------------------------------------------------------
  if("obs_species_variable" %in% dat_names) {
    var <- unique(data$obs_species_variable)
    possible_vars <- coleo_get_attributes_table(column = "variable") |> unlist()
    possible_vars <- c(possible_vars, NA) # Accept NAs in empty campaigns
    are_vars_valid <- all(var %in% possible_vars)

    if(!are_vars_valid) warning("--------------------------------------------------\nV\u00E9rifiez les valeurs ", dput(var[!var %in% possible_vars]), " de la colonne obs_species_variable ou injectez ces valeurs dans la table attributes. Cette colonne contient une valeur qui n'est pas une valeur de la table attributes.\n\n")
  }


  #------------------------------------------------------------------------
  # Check that obs_species_value is 1 when obs_species_variable is "pr\u00E9sence"
  #------------------------------------------------------------------------
  if("obs_species_variable" %in% dat_names) {
    presence_rows <- which(data$obs_species_variable == "pr\u00E9sence")
    if(length(presence_rows) > 0) {
      is_true <- all(data$obs_species_value[presence_rows] == 1)

      if(!is_true) warning("--------------------------------------------------\nV\u00E9rifiez les valeurs de la colonne obs_species_value. Les lignes qui contiennent la valeur \"pr\u00E9sence\" dans la colonne obs_species_variable doivent avoir la valeur 1 dans la colonne obs_species_value.\n\n")
    }
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
    cols_format <- sapply(cols_time, function(x) {
      split <- strsplit(unlist(data[,x]), ":", fixed = TRUE)
      na <- is.na(split)
      # Accept NAs
      # na_in_time <- c(na_in_time, any(na))
      split <- split[!na]
      # If there is no time, return TRUE
      if (length(split) == 0) return(TRUE)
      # Else, check if all time have 3 parts (hour, minute, second)
      time_ndigits <- sapply(split, nchar)
      possibly_time_digits(time_ndigits)
    })
    is_time_format <- all(cols_format)
    if(!all(cols_format)) warning("--------------------------------------------------\nV\u00E9rifiez le format des valeurs d'heure des colonnes ", dput(names(cols_format)[!cols_format]), ". L'heure doit etre du format \"HH:MM:SS\".\n\n")

    # Check if there is NAs ------------------------------------------------
    na_in_time <- sapply(cols_time, function(x) {
      split <- strsplit(unlist(data[,x]), ":", fixed = TRUE)
      na <- is.na(split)
      any(na)
    })
    if("observations_time_obs" %in% names(na_in_time)){
      which_time_obs <- which("observations_time_obs" == names(na_in_time))
      if(na_in_time[which_time_obs]) warning("--------------------------------------------------\nCertaines valeurs de temps sont manquantes ou NA. Les lignes sans valeurs dans la colonne observations_time_obs ne seront pas inject\u00E9es dans la table observations.\n\n")
    }
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
    ## Validate number of digits
    date_digits_message <- coleo_validate_date_digits(data, dat_names, cols_date)
    if (!is.na(date_digits_message)) warning(date_digits_message)

    ## Check if required date columns has NA values
    date_na_message <- coleo_validate_required_date_na(data, cols_date, tbl)
    if (!is.na(date_na_message)) warning(date_na_message)
  }

  #------------------------------------------------------------------------
  # Diagnistics
  #
  # - Check number of empty observations, and dates range
  # - Check number of entries per table
  #------------------------------------------------------------------------
  # Check that the dates values are within a decent range -----------------
  diags_message <- coleo_validate_diagnostics(data, cols_date, no_obs)
  if (!is.na(diags_message)) message(diags_message)

  # Check number of entries per table -------------------------------------
  message("---\n\nRésumé des injections par table :\n")

  req_tbls <- coleo_return_required_tables(campaign_type)
  req_tbls <- req_tbls[!grepl("lookup", req_tbls)]

  ## Loop through tables
  for(table in req_tbls) {
    requests <- data |>
      coleo_injection_prep(db_table = table)
    nvals <- nrow(requests)

    ### Prep message
    message <- paste0(table, " : ", nvals)

    ### Update message with campaigns that need to be injected (végétation_transect only)
    if (campaign_type == "végétation_transect" & table == "campaigns")  message <- new_vegetation_transect_campaigns(data, nvals)

    ### Update message with missing observations
    if ("obs_species_taxa_name" %in% dat_names) {
      if (any(is.na(data$obs_species_taxa_name)) & table == "observations")  message <- missing_obs(data, nvals)
    }

    ### Print message
    message(message)

    ### Unnest data
    ### Add id column
    newname <- sub(table, pattern = "s$", replacement = "")
    name_id <- paste0(newname, "_id")
    ### Add a unique value to each row
    data <- requests |>
      dplyr::select(-inject_request)
    data[,name_id] <- 1:nrow(data)
    ### Select column names to keep
    old_names <- coleo_get_column_names(table)$column_name
    old_names <- old_names[!grepl("_id", old_names)]
    which_old <- names(data) %in% old_names
    ### Make new column names
    new_names <- paste0(table, "_", names(data)[which_old])
    ### Change column names to new names
    names(data)[which_old] <- new_names
    ### Unnest data
    data <- data|>
      dplyr::ungroup()
    if("data" %in% colnames(data)) {
    data <- data |>
      tidyr::unnest(cols = c("data"))
    }
  }

  message("\n==================================================\n\n")

  #------------------------------------------------------------------------
  # Campaign-specific validation
  #------------------------------------------------------------------------
  # if (campaign_type %in% c("v\u00E9g\u00E9tation", "v\u00E9g\u00E9tation_transect")) {
  #   # Check that observations within 100m2 parcelles are only of the 'arbustive' strate
  #   are_right_stratum <- data$observations_stratum[data$efforts_samp_surf == 100] == "arbustive"

  #   if(!all(are_right_stratum)) warning("--------------------------------------------------\nV\u00E9rifiez les valeurs de strate pour les observations des placettes de 100m2. La seule valeur admissible est 'arbustive'.\n\n")


  #   # Check that observations within 400m2 parcelles, that are not extra observations, are only of the 'arborescence' strate
  #   # - All stratum can occur within extra observations
  #   is_extra_obs <- "observations_extra_variable_1" %in% dat_names
  #   if(is_extra_obs) {
  #     are_right_stratum_400 <- data$observations_stratum[data$efforts_samp_surf == 400 & is.na(data$observations_extra_variable_1) & !is.na(data$obs_species_taxa_name)] == "arborescente"
  #   } else {
  #     are_right_stratum_400 <- data$observations_stratum[data$efforts_samp_surf == 400 & !is.na(data$obs_species_taxa_name)] == "arborescente"
  #   }
    
  #   if(exists("are_right_stratum_400")) {
  #     if(!all(are_right_stratum_400, na.rm = TRUE)) warning("--------------------------------------------------\nV\u00E9rifiez les valeurs de strate pour les observations des placettes de 400m2. La seule valeur admissible est 'arborescente', exception faite des observations suppl\u00E9mentaire qui sont identifi\u00E9es dans la colonne 'observations_extra_variable_1'.\n\n")
  #   }
  # }
}



#' Fonction pour calculer le nombre de nouvelles campagnes de transects de végétation
#'
#' Cette fonction calcule le nombre de nouvelles campagnes et de campagnes existantes dans le dataframe,
#' et met à jour le message en conséquence.
#'
#' @param df_id Le dataframe contenant les données à injecter.
#' @param nvals Le nombre total de campagnes.
#'
#' @return Le message mis à jour.
#'
new_vegetation_transect_campaigns <- function(data, nvals){
  veg_campaigns <- coleo_request_general(endpoint = "campaigns", response = TRUE, schema = 'public', "type" = "eq.végétation_transect")
  if (length(veg_campaigns) > 0) {
    veg_campaigns <- subset(veg_campaigns, select = c(id, site_id, opened_at))
    # Add site_code to veg_campaigns
    site_code <- coleo_request_general(endpoint = "sites", response = TRUE, schema = 'public', "id" = paste0("in.(",paste(veg_campaigns$site_id, collapse = ","), ")")) |>
      dplyr::select(id, site_code)
    # Join veg_campaigns and site_code
    veg_campaigns <- veg_campaigns |>
      dplyr::left_join(site_code, by = c("site_id" = "id")) |>
      dplyr::rename(campaign_id = id, campaigns_opened_at = opened_at, sites_site_code = site_code)
    # Check if existing veg_campaigns in data
    df_c_id <- data |>
      dplyr::left_join(veg_campaigns, by = c("sites_site_code", "campaigns_opened_at")) |>
      as.data.frame()
    ## Isolate campaigns that are not yet in coleo
    df <- df_c_id[is.na(df_c_id$campaign_id),] |> subset(select=-campaign_id) |> subset(select=-site_id)

    if (nrow(df) > 0) {
      requests_new <- df |>
        coleo_injection_prep(db_table = 'campaigns')
      nvals_new <- nrow(requests_new)
    } else {
      nvals_new <- 0
    }
  } else {
    nvals_new <- nvals
  }

  ## Calculate number of existing campaigns
  nvals_exist <- nvals - nvals_new

  ### Update message
  message <- paste0("campaigns", " : ", nvals_new, " (", nvals_exist, " campagnes existent d\u00E9j\u00E0 dans coleo)")

  return(message)
}


#' Fonction pour calculer le nombre d'observations qui échoueront lors de l'injection
#'
#' Cette fonction calcule le nombre d'observations qui échoueront lors de l'injection
#' et met à jour le message en conséquence.
#'
#' @param df_id Le dataframe contenant les données à injecter.
#' @param nvals Le nombre total de campagnes.
#'
#' @return Le message mis à jour.
#'
missing_obs <- function(data, nvals){
  # Check if there are missing observations
  na_obs <- sum(is.na(data$obs_species_taxa_name), na.rm = TRUE)
  nvals_new <- nvals - na_obs

  ### Update message
  message <- paste0("observations", " : ", nvals_new, " (", na_obs, " lignes sans taxon observé entraineront un \u00E9chec d'injection)")

  return(message)
}


#' Validation du format des dates
#'
#'
#' @param data Le dataframe contenant les données à injecter.
#' @param dat_names Les noms des colonnes du dataframe.
#' @param cols_date Les noms des colonnes contenant des date.
#'
#' @return Le message de validation.
#'
coleo_validate_date_digits <- function(data, dat_names, cols_date) {
  # Initiate message
  message <- NA

  # Helper function to check if a date has the right number of digits
  has_valid_digits <- function(date) {
    date_parts <- strsplit(date, "-", fixed = TRUE)[[1]]
    all(nchar(date_parts) == c(4, 2, 2))
  }

  # Check date columns
  cols_ndigits <- sapply(cols_date, function(x) {
    dates <- data[[x]]
    non_na_dates <- dates[!is.na(dates)]
    
    # Check if all dates have 3 parts (year, month, day)
    all_dates_valid <- all(sapply(non_na_dates, function(date) sum(length(strsplit(date, "-", fixed = TRUE)[[1]])) == 3))
    
    # Check if all dates have the right number of digits
    all_digits_valid <- all(sapply(non_na_dates, has_valid_digits))
    
    all_dates_valid && all_digits_valid
  })
  is_ndigits_valid <- all(cols_ndigits)


  if(!is_ndigits_valid) message <- "--------------------------------------------------\nV\u00E9rifiez le format des valeurs de dates. Les dates doivent \u00EAtre du format YYYY-MM-DD.\n\n"

  return(message)
}


#' Validation de la présence de valeurs NA dans les colonnes de date
#'
#'
#' @param data Le dataframe contenant les données à injecter.
#' @param cols_date Les noms des colonnes contenant des date.
#' @param tbl La table de la base de données à laquelle les données seront injectées.
#'
#' @return Le message de validation.
#'
coleo_validate_required_date_na <- function(data, cols_date, tbl) {
  # Initiate message
  message <- NA
    
  all_na <- coleo_validate_empty_cols(data, cols_date)

  non_na_date_cols <- cols_date[!all_na]
  req_cols <- tbl[tbl$colonne_requise==TRUE,]$noms_de_colonnes
  is_na <- any(is.na(data[cols_date[cols_date %in% req_cols]])) | any(is.na(data[non_na_date_cols]))


  if(is_na) message <- ("--------------------------------------------------\nCertaines valeurs de date sont manquantes ou NA. Les lignes sans valeurs dans les colonnes campaigns_opened_at, observations_date_obs ou remote_sensing_events_date_start ne seront pas injectées dans leurs tables respectives.\n\n")

  return(message)
}


#' Validation des colonnes vides
#'
#'
#' @param data Le dataframe contenant les données à injecter.
#' @param columns Les noms des colonnes à valider.
#'
#' @return Le vecteur booléen représentant si la colonne est vide.
#'
coleo_validate_empty_cols <- function(data, columns) {
  all_na <- sapply(columns, function(x) {
    cols <- data[[x]]
    non_na_cols <- cols[!is.na(cols)]
    length(non_na_cols) == 0
  })
  
  return(all_na)
}


#' Validation de la présence de valeurs NA dans les colonnes de date
#'
#'
#' @param data Le dataframe contenant les données à injecter.
#' @param cols_date Les noms des colonnes contenant des dates.
#' @param no_obs Le nombre d'observations sans taxon.
#'
#' @return Le message de validation.
#'
coleo_validate_diagnostics <- function(data, cols_date, no_obs = 0) {
  dates <- unlist(data[cols_date])
  dates <- dates[!is.na(dates)]
  # Extract date parts for valid dates
  split <- strsplit(dates, "-", fixed = TRUE)
  split <- split[sapply(split, length) == 3]
  if (length(split) == 0) return(NA)
  # Year
  range_year <- range(as.numeric(sapply(split, `[[`, 1)))
  # Month
  range_month <- range(as.numeric(sapply(split, `[[`, 2)))
  # Day
  range_day <- range(as.numeric(sapply(split, `[[`, 3)))

  message <- paste0("==================================================\n\nValidation diagnostique :\n",
  if ("obs_species_taxa_name" %in% names(data)) paste0("\n- V\u00E9rifiez les lignes qui repr\u00E9sentent des campagnes vides : il y a ", no_obs, " lignes sans observations. Celles-ci entraineront une erreur lors de l'injection des observations.\n"),
  "\n- V\u00E9rifiez que l'intervalle des dates", paste0(" inject\u00E9", "es "), "correspond aux attentes. Les valeurs de dates des colonnes ", paste0(cols_date, collapse = ", "), " se trouvent dans l'intervalle de", paste0(" l'ann\u00E9", "e "), range_year[1], " \u00E0 ", range_year[2], " du mois ", range_month[1], " \u00E0 ", range_month[2], " et du jour ", range_day[1], "  \u00E0 ", range_day[2], ".\n\n- Si les", paste0(" donn\u00E9", "es"), " sont bonnes et qu'aucun autre message n'apparait, vous pouvez", paste0(" proc\u00E9", "der"), " \u00e0 l'injection des", paste0(" donn\u00E9", "es."), '\n')

  return(message)
}

#' Valider les Coordonnées
#'
#' Cette fonction valide les coordonnées dans un jeu de données donné pour s'assurer qu'elles se situent dans des plages valides du système de coordonnées géographique EPSG:4326.
#'
#' @param data Un dataframe contenant les coordonnées à valider.
#' @param dat_names Un vecteur de caractères des noms de colonnes dans le dataframe qui contiennent les valeurs de latitude et de longitude.
#' @param bbox Un vecteur de 4 éléments représentant la boîte englobante des coordonnées géographiques. Par défaut, la boîte englobante est [-90, 90, -180, 180].
#'
#' @details
#' La fonction valide que les valeurs de latitude sont dans la plage [-90, 90] et que les valeurs de longitude sont dans la plage [-180, 180].
#'
#' @return Cette fonction retourne un message d'avertissement si des coordonnées sont hors des plages valides.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(lat = c(45, 50, 60), lon = c(-70, -75, -80))
#' dat_names <- c("lat", "lon")
#' bbox <- c(-90, 90, -180, 180)
#' coleo_validate_coordinates(data, dat_names)
#' }
coleo_validate_coordinates_projection <- function(data, dat_names, bbox = c(-90, 90, -180, 180)) {
  lat_names <- grepl("lat", dat_names)
  lon_names <- grepl("lon", dat_names)

  lat_error = lon_error = FALSE

  if (any(lat_names)) {
    lat_range <- unlist(data[,lat_names]) |> range(na.rm = TRUE)
    if (lat_range[2] > bbox[2] | lat_range[1] < bbox[1]) lat_error <- TRUE
  }
  if (any(lon_names)) {
    lon_range <- unlist(data[,lon_names]) |> range(na.rm = TRUE)
    if (lon_range[2] > bbox[4] | lon_range[1] < bbox[3]) lon_error <- TRUE
  }
  
  message <- NA
  if (lat_error | lon_error) message <- paste0("--------------------------------------------------\nV\u00E9rifiez la projection des coordonnées pour qu'elles soient en EPSG:4326. Des valeurs ne respectent pas le système de coordonnées géographiques requis.\n\n")

  return(message)
}

#' Valider les Coordonnées
#'
#' Cette fonction valide les coordonnées dans un jeu de données donné pour s'assurer qu'elles se situent dans la boîte englobante du Québec.
#'
#' @param data Un dataframe contenant les coordonnées à valider.
#' @param dat_names Un vecteur de caractères des noms de colonnes dans le dataframe qui contiennent les valeurs de latitude et de longitude.
#' @param bbox Un vecteur de 4 éléments représentant la boîte englobante des coordonnées géographiques. Par défaut, la boîte englobante du Québec est [-79.76, 44.99, -57.10, 62.59].
#'
#' @details
#' La fonction vérifie que les valeurs de latitude et de longitude se situent dans la boîte englobante du Québec (xmin: -79.76, ymin: 44.99, xmax: -57.10, ymax: 62.59).
#'
#' @return Cette fonction retourne un message d'avertissement si des coordonnées sont hors des plages valides.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(lat = c(45, 50, 60), lon = c(-70, -75, -80))
#' dat_names <- c("lat", "lon")
#' bbox <- c(-79.76, 44.99, -57.10, 62.59)
#' coleo_validate_coordinates(data, dat_names, bbox)
#' }

coleo_validate_coordinates <- function(data, dat_names, bbox = c(-79.76, 44.99, -57.10, 62.59)) {
  lat_names <- grepl("lat", dat_names)
  lon_names <- grepl("lon", dat_names)
  
  lat_error = lon_error = FALSE

  if (any(lat_names)) {
    lat_range <- unlist(data[,lat_names]) |> range(na.rm = TRUE)
    if (lat_range[2] > bbox[4] | lat_range[1] < bbox[2]) lat_error <- TRUE
  }
  if (any(lon_names)) {
    lon_range <- unlist(data[,lon_names]) |> range(na.rm = TRUE)
    if (lon_range[2] > bbox[3] | lon_range[1] < bbox[1]) lon_error <- TRUE
  }

  message <- NA
  if (lat_error | lon_error) message <- paste0("--------------------------------------------------\nV\u00E9rifiez les coordonnées. Certaines valeurs se trouvent à l'extérieur du Québec.\n\n")

  return(message)
}