



#' Preparer les données pour injection
#'
#' @param df le dataframe à preparer
#' @param db_table la table ciblée dans la base de données
#'
#' @return un tbl, nested
#' @export
coleo_prep_input_data <- function(df, db_table) {

    # Is there extra columns
  is_there_extra <- grepl(paste0(db_table, "_extra"), names(df))
  if (any(is_there_extra)) {
    df <- coleo_format_extra_col(df, db_table, extraCols = names(df)[is_there_extra])
  }

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
coleo_format_extra_col <- function(df, db_table, extraCols) {

    df <- tibble::as_tibble(df)

    df$extra <- jsonlite::toJSON(NA_character_)

     extra_col_groups <- split(extraCols, strsplit(extraCols, "_") |> 
      purrr::map_chr(tail, 1))
   

    # Rowwise -------------------------------------------------------------
    for (i in 1:nrow(df)) {


        # Group -----------------------------------------------------------
        extra_list <- list()
        j = 1
        for (extra_group in seq_along(extra_col_groups)) {

            # Group
            group <- unlist(extra_col_groups[extra_group])

            ## Subset df
            df_group <- df[i, group]

            ## Select variable
            variable <-
                df_group[,grepl(paste0(db_table, "_extra_variable_", extra_group), names(df_group))] |>
                unlist()
            if (is.null(variable) | is.na(variable)) next
            
            ## Select fields that aren't the variable
            fields <- stringr::str_split(group, "_", simplify = TRUE)[,3]
            fields <- fields[!fields %in% "variable"]

            ## Select values into a named list
            values <- df_group[,!grepl("variable", names(df_group))] 
            list_group <- sapply(values, list)
            ### Remove NA's
            is_values_na <- !is.na(list_group)
            list_group <- list_group[is_values_na]
            fields <- fields[is_values_na]
            names(list_group) <- fields

            ## Nest variable list
            ### Skip if empty
            if(length(list_group) > 0) {
              extra_list[j] <- list(list_group)
              names(extra_list)[j] <- variable

              j = j + 1
            }
        }

        df$extra[i] <- jsonlite::toJSON(extra_list, auto_unbox = TRUE)
    }


    # Clean df
    names(df)[grepl("^extra$", names(df))] <- paste0(db_table, "_extra")
    out <- df[,!grepl(paste0(db_table, "_extra_"), names(df))]

    return(out)
}
