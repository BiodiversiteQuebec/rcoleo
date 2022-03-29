# functions that work with coleo_get_name_table()

#' Retourne un vecteur contenant les noms valides de campagnes
#'
#'
#' @return character vector of all valid campaign types
#' @export
#'
coleo_return_valid_campaigns <- function(){
  camp_types_resp <- coleo_request_general(enum = "enum_campaigns_type", endpoint = "enum_options")
  campaigns <- unlist(httr2::resp_body_json(camp_types_resp), use.names = FALSE)

  return(campaigns)
}


#' Retourne un vecteur contenant les noms valides de type de site
#'
#'
#' @return character vector of all valid site types
#' @export
#'
coleo_return_valid_site_types <- function(){
  site_types_resp <- coleo_request_general(enum = "enum_sites_type", endpoint = "enum_options")
  legal_vals <- unlist(httr2::resp_body_json(site_types_resp), use.names = FALSE)

  return(legal_vals)
}


# Retoune les noms d'une colonne de coleo_get_name_table() pour une table
## db_table : le nom d'une table de la bd
## column : le nom de colone de la coleo_get_name_table() ("table", "input_column", "db_column", "is_required", "required_class", "legal_values")
coleo_get_name_table_column <- function(db_table, column = "input_column"){

  nm_tbl <- coleo_get_name_table()

  assertthat::assert_that(db_table %in% unique(nm_tbl$table),
                          msg = "Not one of the database tables")

  assertthat::assert_that(column %in% names(nm_tbl),
                          msg = paste0("column should be one of: ",
                                       paste(names(nm_tbl), collapse = ", ")
                                       ))

  input_names <- split(nm_tbl[[column]], nm_tbl$table)

  out <- input_names[[db_table]]

  # let there be ID columns when needed

  if (db_table == "observations") {
    out <- c(out, "campaign_id")
  }

  if (db_table == "sites"){
    out <- c(out, "cell_id")
  }

  return(out)

}

# Retoune un vecteur des noms valides de colonnes du jeu de données pour une table de la base de données
## db_table : le nom d'une table de la bd
coleo_get_rename_vec_input_to_db <- function(db_table){
  resp_cols <- coleo_request_general(table = tbl, endpoint = "table_columns")
  cols_db <- purrr::map_dfr(httr2::resp_body_json(resp_cols), as.data.frame)

  valid_col_names <- paste(db_table, cols_db$column_name, sep = "_")

  return(valid_col_names)
}


# Retourne les noms valides de colonnes du jeu de données REQUIS pour une table de la base de données
## db_table : le nom d'une table de la bd
coleo_get_required_name_table <- function(db_table) {

  nm_tbl <- coleo_get_name_table()

  assertthat::assert_that(db_table %in% unique(nm_tbl$table),
                          msg = "Not one of the database tables")

  out <- subset(nm_tbl, is_required & table == db_table)[,"input_column"][[1]]

  return(out)
}
