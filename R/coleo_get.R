########################
# Fontions qui performent des requêtes sur coleo
# - pour utilisation interne
########################

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

  # Ajouter lon/lat si geom présent
  geom_cols <- grepl("geom",valid_col_names)
  geoms <- strsplit(valid_col_names[geom_cols], split = "_")
  geom_tables <- sapply(seq_along(geoms), function(x) geoms[[x]][1])
  for(tbl in geom_tables) {
    valid_col_names <- c(valid_col_names, paste(tbl,"lat", sep = "_"))
    valid_col_names <- c(valid_col_names, paste(tbl,"lon", sep = "_"))
  }

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


#' Retourne la table attributes ou les valeurs d'un champ si spécifié
#'
#' @param column un champ de la table attributes
#'
#' @return un data frame de la table attributes ou des valeurs contenues dans un champ
coleo_get_attributes_table <- function(column = NULL) {
  resp_json <- coleo_request_general(endpoint = "attributes") |>
    httr2::resp_body_json() |>
    replace_null() # Valeurs NULL empêchent la transformation en df
    attributes_df <- purrr::map_dfr(resp_json, as.data.frame)

  if(!is.null(column)) {
    out <- attributes_df[,column]
  } else {
    out <- attributes_df
  }

  return(out)
}

## Helper function to remove null values from json request responses
replace_null <- function(x){
  x <- purrr::map(x, ~ replace(.x, is.null(.x), NA_character_))
  purrr::map(x, ~ if(is.list(.x)) replace_null(.x) else .x)
}
