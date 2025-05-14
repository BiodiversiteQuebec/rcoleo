########################
# Fontions qui performent des requêtes sur coleo
# - pour utilisation interne
########################

## convenience functions for extracting the columns of a table, and the types of a column

coleo_get_column_names <- function(tbl){
  coleo_request_general('rpc/table_columns', response_as_df = TRUE,'table_name' = tbl)
}


coleo_get_enum_values <- function(enum_col_name){
  coleo_request_general('rpc/get_enum_values', 'enum_type' = enum_col_name, response_as_df=TRUE) |> 
    unlist(use.names = FALSE)
}


#' Retourne la table attributes ou les valeurs d'un champ si spécifié
#'
#' @param column Un champ de la table attributes
#'
#' @return un data frame de la table attributes ou des valeurs contenues dans un champ
coleo_get_attributes_table <- function(column = NULL) {
  attributes_df <- coleo_request_general('attributes', response_as_df = TRUE, 'schema'='public')

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
