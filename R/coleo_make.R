
#' Fonction pour standardiser la production des noms de colonnes des jeux de données à importer.
#'
#' Les noms de colonne sont en 'snake_case' pour en faciliter la tranformation par le système interne.
#'
#'
#' @param db_table une table de la base de données.
#' @param db_colname les noms de champs de la table.
#'
#' @return Un vecteur contenant les noms de colonnes tels qu'utilisés dans le gabarit d'injection.
#'
#' @export
coleo_make_df_column_names <- function(db_table, db_colname) {
  valid_col_names <- paste(db_table, db_colname, sep = "_")
  return(valid_col_names)
}
