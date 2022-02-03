



#' Preparer les données pour injection
#'
#' @param df le dataframe à preparer
#' @param db_table la table ciblée dans la base de données
#'
#' @return un tbl, nested
#' @export
coleo_prep_input_data <- function(df, db_table){

  input_fields <- coleo_get_name_table_column(db_table)

  not_in_table <- setdiff(names(df), input_fields)

  if(db_table != "observations") {
    df_info_only <- df %>%
      tidyr::nest(data = any_of(not_in_table))
  } else {
    message("nesting is not necessary for those tables")
    df_info_only <- df
  }

  # rename the dataset
  rename_vec <- coleo_get_rename_vec_input_to_db(db_table)

  df_info_renamed <- df_info_only %>%
    dplyr::rename(any_of(rename_vec))

  #is there a lat or lon??
  is_there_latlon <- sum(names(df_info_renamed) %in% c("lat", "lon")) == 2

  if (is_there_latlon) {
    df_info_renamed <- format_spatial(df_info_renamed)
  }

  return(df_info_renamed)
}
