



#' Preparer les données pour injection
#'
#' @param df le dataframe à preparer
#' @param db_table la table ciblée dans la base de données
#'
#' @return un tbl, nested
#' @export
coleo_prep_input_data <- function(df, db_table) {
  input_fields <- coleo_return_name_table(db_table)

  names_present <- intersect(input_fields, names(df))

  # find and also preserve any columns that end in "_id".
  # use API to get the actual ID columns needed
  colnames_of_tbl <- coleo_get_column_names(tbl = db_table)$column_name
  id_names <- colnames_of_tbl[grepl(pattern = ".*_id", x = colnames_of_tbl)]

  nesting_names <- c(id_names, names_present)

  # Critical to not nest observations!
  if (db_table != "observations") {
    df_info_only <- df |>
      dplyr::nest_by(dplyr::across(dplyr::any_of(nesting_names)))
    # test with nest_by
  } else {
    message("L'imbrication n'est pas necessaire pour cette table")
    df_info_only <- dplyr::rowwise(df)
  }

  # Is there extra columns
  is_there_extra <- grepl("extra", names(df_info_only))
  if (any(is_there_extra)) {
    df_info_only <- coleo_format_extra_col(df_info_only, db_table)
    df_info_only <- dplyr::rowwise(df_info_only)
  }

  # rename the dataset
  rename_vec <- coleo_return_rename_vec_input_to_db(db_table)

  df_info_renamed <- df_info_only |>
    dplyr::rename(dplyr::any_of(rename_vec))

  # is there a lat or lon??
  is_there_lat <- grepl("lat", names(df_info_renamed))
  is_there_lon <- grepl("lon", names(df_info_renamed))
  is_there_latlon <- sum(is_there_lat, is_there_lon) == 2

  # Rename lat/lon cols
  names(df_info_renamed)[is_there_lat] <- "lat"
  names(df_info_renamed)[is_there_lon] <- "lon"

  if (is_there_latlon) {
    df_info_renamed <- format_spatial(df_info_renamed)
  }

  return(df_info_renamed)
}




#' Preparer la colonne extra pour injection
#'
#' @param df le dataframe à preparer
#' @param db_table la table ciblée dans la base de données
#'
#' @return un tbl
coleo_format_extra_col <- function(df, db_table) {

  df <- tibble::as_tibble(df)
  # Rename comlumns of interest
  names(df)[grepl(paste0(db_table, "_extra_variable"), names(df))] <- "extra_variable"
  names(df)[grepl(paste0(db_table, "_extra_type"), names(df))] <- "extra_type"
  names(df)[grepl(paste0(db_table, "_extra_description"), names(df))] <- "extra_description"
  names(df)[grepl(paste0(db_table, "_extra_value"), names(df))] <- "extra_value"
  names(df)[grepl(paste0(db_table, "_extra_units"), names(df))] <- "extra_units"

  # variable and value are minimally required
  if (!"extra_type" %in% names(df)) df$extra_type <- NA_character_
  if (!"extra_units" %in% names(df)) df$extra_units <- NA_character_
  if (!"extra_description" %in% names(df)) df$extra_description <- NA_character_

  df$extra <- jsonlite::toJSON(NA_character_)
  for (i in 1:nrow(df)) {
    if (is.null(df$extra_variable[i]) | is.na(df$extra_variable[i])) next

    # Identify the variable
    variable <- df$extra_variable[i]
    # Select only columns of itnerest
    colsOI <- df[i, grepl("extra_", names(df))]
    colsOI <- subset(colsOI, select = -extra_variable)
    colsOI <- colsOI[, !is.na(colsOI)[1, ]]

    # Join extra columns into a list object
    cols <- sapply(names(colsOI), function(x) strsplit(x, "extra_")[[1]][2])
    list <- list()
    for (col in seq_along(cols)) {
      list_to_add <- list(unlist(colsOI[, col], use.names = FALSE))
      names(list_to_add) <- cols[col]
      list <- append(list, list_to_add)
    }

    # Transform to a named nested list
    extra_list <- list(list)
    names(extra_list) <- variable

    # Make it into a json object
    df$extra[i] = jsonlite::toJSON(extra_list, auto_unbox = TRUE)
  }

  # Clean df
  out <- df[,!grepl("extra_", names(df))]
  names(out)[grepl("^extra$", names(df))] <- paste0(db_table, "_extra")

  return(out)
}



