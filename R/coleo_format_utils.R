#' Formatage automatisé des colonnes d'un jeu de données à injecter dans coleo
#'
#' Cette fonction formate les colonnes à la bonne classe. Elle est appelée par
#' les fonctions \code{coleo_read_template} et \code{coleo_read_csv}.
#'
#' @param dataFrame un jeu de données
#'
#' @export
#' 
coleo_format <- function(dataFrame) {
  # Get required columns for the campaign type
  campaign_type <- coleo_return_campaign_type(dataFrame)
  data_cols <- coleo_return_cols(campaign_type)

  # Format columns
  for(col in seq_along(names(dataFrame))) {
    true_col_nm <- names(dataFrame)[col]

    if(!true_col_nm %in% data_cols$noms_de_colonnes) {
      # Extra columns must remain as characters
      if (grepl("_extra", true_col_nm)) next
      ## Some columns may have modified names
      ## eg. lures_lure_1
      mod_col_nm <- sapply(data_cols$noms_de_colonnes,
        function(x) grepl(x, true_col_nm)
      )
      if (any(mod_col_nm)) {
        true_col_nm <- names(which(mod_col_nm))
        ## Select the longuest name - In case there is multiple matching
        true_col_nm <- true_col_nm[which.max(nchar(true_col_nm))]
      } else next
    }

    colClass <- data_cols[data_cols$noms_de_colonnes == true_col_nm, "classe"]

    ## Special cases
    if (colClass == "list") {
      ### Elements within a list are separated by comas
      dataFrame[,col] <- list(stringr::str_split(dataFrame[,col], ", |,"))
    } else {
      ### Decimals are separated by a point
      if (colClass == "numeric") dataFrame[,col] <- gsub(",", ".", dataFrame[,col])
      
      ### Format columns
      asClass <- paste0('as.', colClass)
      dataFrame[,col] <- .Primitive(asClass)(dataFrame[,col], drop = FALSE)
    }
  }

  return(dataFrame)
}



#' Formate les donnees avec leurs cordonnées spatiales.
#'
#' La localisation des données doivent être formatées correctement pour être
#' injectées dans coleo.
#'
#' Cette fonction est appelée par \code{\link[rcoleo]{coleo_prep_input_data}}.
#'
#' @param df_to_inject data.frame à injecter
#'
#' @importFrom rlang .data
#' @export
format_spatial <- function(df_to_inject) {

  ## dataframe must have lat and lon columns
  assertthat::assert_that(assertthat::has_name(df_to_inject, "lon"))
  assertthat::assert_that(assertthat::has_name(df_to_inject, "lat"))
  assertthat::assert_that(inherits(df_to_inject, "rowwise_df"))


  formatted_input_data <- df_to_inject |>
    dplyr::mutate(geoj = list(geojsonio::geojson_list(c(lon = lon,
                                                        lat =  lat),
                                                      lat = "lat", lon = "lon")),
                  feat = list(geoj[["features"]]),
                  feat = list(purrr::flatten(feat)),
                  geom = list(feat[["geometry"]]))

  keep_these <- which(! names(formatted_input_data) %in% c("geoj", "feat", "lat", "lon") )

  formatted_input_data <- formatted_input_data[, keep_these]

  return(formatted_input_data)
}


#' Prepare la colonne extra pour injection.
#'
#' Les colonnes extra doivent être formatées en json pour être
#' injectées dans coleo.
#'
#' Cette fonction est appelée par \code{coleo_prep_input_data}.
#'
#' @param df le dataframe à preparer.
#' @param db_table la table ciblée dans la base de données.
#'
#' @return un tbl
coleo_format_extra_col <- function(df, db_table) {

  # Is there extra columns
  is_there_extra <- grepl(paste0(db_table, "_extra"), names(df))
  if (!any(is_there_extra)) return(df)
  extraCols <- names(df)[is_there_extra]

  df <- tibble::as_tibble(df)

  df$extra <- jsonlite::toJSON(NA_character_)

  extra_col_groups <- split(extraCols, strsplit(extraCols, "_") |>
    purrr::map_chr(tail, 1))


  # Rowwise -------------------------------------------------------------
  for (i in 1:nrow(df)) {


    # Group -----------------------------------------------------------
    extra_list <- list()
    j <- 1
    for (extra_group in seq_along(extra_col_groups)) {

      # Group
      group <- unlist(extra_col_groups[extra_group])

      ## Subset df
      df_group <- df[i, group]

      ## Select variable
      variable <-
        df_group[, grepl(paste0(db_table, "_extra_variable_", extra_group), names(df_group))] |>
        unlist()
      if (is.null(variable) | is.na(variable)) next

      ## Select fields that aren't the variable
      fields <- stringr::str_split(group, "_", simplify = TRUE)[, 3]
      fields <- fields[!fields %in% "variable"]

      ## Select values into a named list
      values <- df_group[, !grepl("variable", names(df_group))]
      list_group <- sapply(values, list)
      ### Replace NA's with nulls
      is_values_na <- is.na(list_group)
      list_group[is_values_na] <- ""
      names(list_group) <- fields

      ## If there is a nested list, flatten.
       # this is a workaround for the behaviour of rowwise() when dealing with list-columns
      is_list <- sapply(list_group, is.list)
      for (field in which(is_list)) {
        list_group[field] <- list_group[field][[1]]
      }

      ## Nest variable list
      ### Skip if empty
      if (length(list_group) > 0) {
        extra_list[j] <- list(list_group)
        names(extra_list)[j] <- variable

        j <- j + 1
      }
    }

    df$extra[i] <- jsonlite::toJSON(extra_list, auto_unbox = TRUE)
  }


  # Clean df
  names(df)[grepl("^extra$", names(df))] <- paste0(db_table, "_extra")
  out <- df[, !grepl(paste0(db_table, "_extra_"), names(df))]

  return(out)
}
