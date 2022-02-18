# functions that work with coleo_get_name_table()

#' Retourne un vecteur contenant les noms valides de campagnes
#'
#'
#' @return character vector of all valid campaign types
#' @export
#'
coleo_return_valid_campaigns <- function(){
  full_tbl <- coleo_get_name_table()

  legal_vals <- subset(full_tbl, table == "campaigns" & input_column == "campaign_type")[["legal_values"]][[1]]

  return(legal_vals)
}


#' Retourne un vecteur contenant les noms valides de type de site
#'
#'
#' @return character vector of all valid site types
#' @export
#'
coleo_return_valid_site_types <- function(){
  full_tbl <- coleo_get_name_table()

  legal_vals <- subset(full_tbl, table == "sites" & input_column == "site_type")[["legal_values"]][[1]]

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

  if (db_table == "observations") {
    out <- c(out, "campaign_id")
  }

  return(out)

}

# Retoune un vecteur des noms valides de colonnes du jeu de données pour une table de la base de données
## db_table : le nom d'une table de la bd
coleo_get_rename_vec_input_to_db <- function(db_table){
  inputs <- coleo_get_name_table_column(db_table)
  upload <- coleo_get_name_table_column(db_table, column = "db_column")
  out <- purrr::set_names(inputs, upload)

  return(out)
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
