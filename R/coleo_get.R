
# Retoune un vecteur des noms valides de colonnes d'un jeu de données, nommés selon le nom de colonne de la table de la bd, pour une table de la base de données
## db_table : le nom d'une table de la bd
coleo_get_rename_vec_input_to_db <- function(db_table){
  # Nom de colonne du jeu de données
  resp_cols <- coleo_get_column_names(db_table)
  df_col_names <- coleo_make_df_column_names(db_table, resp_cols$column_name)
  # Nom de colonne de la table de la bd
  db_col_names <- resp_cols$column_name
  # Assembler en un vecteur nommé
  names(df_col_names) <- db_col_names


  return(df_col_names)
}


# Retoune un vecteur des noms valides de colonnes du jeu de données pour une table de la base de données
## db_table : le nom d'une table de la bd
coleo_get_name_table <- function(db_table){
  resp_cols <- coleo_get_column_names(db_table)
  valid_col_names <- coleo_make_df_column_names(db_table, resp_cols$column_name)

  return(valid_col_names)
}


# Retourne les noms valides de colonnes du jeu de données REQUIS pour une table de la base de données
## db_table : le nom d'une table de la bd
coleo_get_required_name_table <- function(db_table) {
  nm_tbl <- coleo_get_column_names(db_table)
  req_nm <- subset(nm_tbl, is_nullable == "NO")

  req_col_names <- coleo_make_df_column_names(db_table, req_nm$column_name)

  return(req_col_names)
}


## convenience functions for extracting the columns of a table, and the types of a column

coleo_get_column_names <- function(tbl){
  resp_cols <- coleo_request_general(table = tbl, endpoint = "table_columns")
  cols_df <- purrr::map_dfr(httr2::resp_body_json(resp_cols), as.data.frame)
  return(cols_df)
}


coleo_get_enum_values <- function(enum_col_name){
  resp_enum <- coleo_request_general(enum = enum_col_name, endpoint = "enum_options")
  resp_chr <- httr2::resp_body_json(resp_enum) |> purrr::flatten() |> purrr::flatten_chr()
  return(resp_chr)
}

