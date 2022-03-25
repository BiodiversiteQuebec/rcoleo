########################
# Fontions qui performent des requêtes sur coleo
# - pour utilisation interne
########################

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
