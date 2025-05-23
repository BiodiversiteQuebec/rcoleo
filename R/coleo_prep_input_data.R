#' Preparer les données pour injection
#'
#' Est utilisé par \code{\link[rcoleo]{coleo_injection_prep}} pour formater
#' les données à injecter et les données à transformer (géométries,
#' colonnes extra). Imbrique et isole des colonnes à injecter, place les autres
#' dans une colonne 'data'.
#' 
#' Cette colonne fait appel aux fonctions de formatage
#' \code{\link[rcoleo]{coleo_format_extra_col}} et
#' \code{\link[rcoleo]{format_spatial}}.
#'
#' @param df le dataframe à preparer
#' @param db_table la table ciblée dans la base de données
#' @param schema le schéma de la base de données
#'
#' @return un tbl, nested
#' @export
coleo_prep_input_data <- function(df, db_table, schema = "public") {

  # Convert character NAs to actual NAs
  # df[df == "NA"] <- NA

  # Add cell_id to required table
  if (db_table == "sites" | db_table == "remote_sensing_events") {
    df <- df |>
      dplyr::nest_by(cells_cell_code) |>
      dplyr::mutate(coleo_id = list(coleo_request_by_code(human_code = cells_cell_code, table = "cells", schema = schema)),
            cell_id = coleo_extract_id(coleo_id)) |>
      dplyr::select(-cells_cell_code, -coleo_id) |>
      dplyr::relocate(cell_id) |>
      tidyr::unnest(cols = c(data)) |>
      dplyr::ungroup() |>
      suppressMessages()
  }

  # Add remote_sensing_indicator_id to required table
  if (db_table == "remote_sensing_events") {
    df <- df |>
      dplyr::nest_by(remote_sensing_indicators_name) |>
      dplyr::mutate(coleo_id = list(coleo_request_by_code(human_code = remote_sensing_indicators_name, table = "remote_sensing_indicators", schema = schema)),
            remote_sensing_indicator_id = coleo_extract_id(coleo_id)) |>
      dplyr::select(-remote_sensing_indicators_name, -coleo_id) |>
      dplyr::relocate(remote_sensing_indicator_id) |>
      tidyr::unnest(cols = c(data)) |>
      dplyr::ungroup() |>
      suppressMessages()
  }

  # Campaigns table specific manipulations
  if (db_table %in% c("campaigns", "vegetation_phenology")) {
    sites_schema <- ifelse(schema != "coleo_test", "public", schema) # sites table is in public schema, except for tests
  
    ## Add site_id to campaigns table
    df <- df |>
      dplyr::nest_by(sites_site_code) |>
      dplyr::mutate(coleo_id = list(coleo_request_by_code(human_code = sites_site_code, table = "sites", schema = sites_schema)),
            site_id = coleo_extract_id(coleo_id)) |>
      dplyr::select(-sites_site_code, -coleo_id) |>
      dplyr::relocate(site_id) |>
      tidyr::unnest(cols = c(data)) |>
      dplyr::ungroup() |>
      suppressMessages()
  }

  # Format extra columns
  df <- coleo_format_extra_col(df, db_table)

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
    df_info_only <- dplyr::rowwise(df)
  }

  # rename the dataset
  rename_vec <- coleo_return_rename_vec_input_to_db(db_table)

  df_info_renamed <- df_info_only |>
    dplyr::rename(dplyr::any_of(rename_vec))

  # is there a lat or lon??
  is_there_lat <- grepl("lat", names(df_info_renamed))
  is_there_lon <- grepl("lon", names(df_info_renamed))
  is_there_latlon <- sum(is_there_lat, is_there_lon) == 2

  if (is_there_latlon) {
    # Rename lat/lon cols
    names(df_info_renamed)[is_there_lat] <- "lat"
    names(df_info_renamed)[is_there_lon] <- "lon"

    df_info_renamed <- format_spatial(df_info_renamed)
  }

  return(df_info_renamed)
}
