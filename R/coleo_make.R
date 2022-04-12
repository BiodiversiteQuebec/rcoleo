
#'  Fonction pour standardiser la production les noms de colonnes des jeux de données à importer
#'
#'
#' @param db_table une table de la base de données.
#' @param db_colname les noms de champs de la table.
#'
#' @return
#' @export
coleo_make_df_column_names <- function(db_table, db_colname) {
  valid_col_names <- paste(db_table, db_colname, sep = "_")
  return(valid_col_names)
}

