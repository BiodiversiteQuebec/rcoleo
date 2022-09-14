########################
# Fonctions qui retournent des données formatées issues de requêtes à coleo
########################


# Retoune un vecteur des noms valides de colonnes d'un jeu de données, nommés selon le nom de colonne de la table de la bd, pour une table de la base de données
## db_table : le nom d'une table de la bd
coleo_return_rename_vec_input_to_db <- function(db_table){
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
coleo_return_name_table <- function(db_table){
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
coleo_return_required_name_table <- function(db_table) {
  nm_tbl <- coleo_get_column_names(db_table)
  req_nm <- subset(nm_tbl, is_nullable == "NO")

  req_col_names <- coleo_make_df_column_names(db_table, req_nm$column_name)

  return(req_col_names)
}



#' Retourne un vecteur contenant les noms valides de campagnes
#'
#'
#' @return vecteur de charactères contenant tous les types de capagnes valides
#' @export
#'
coleo_return_valid_campaigns <- function(){
  campaigns <- coleo_get_enum_values("enum_campaigns_type")

  return(campaigns)
}


#' Retourne un vecteur contenant les noms valides de type de site
#'
#'
#' @return vecteur de charactères contenant tous les types de sites valides
#' @export
#'
coleo_return_valid_site_types <- function(){
  legal_vals <- coleo_get_enum_values("enum_sites_type")

  return(legal_vals)
}


#' Trouver les tables requises pour un type de campagne donné
#'
#'
#' @param camp_type un type de campagne valide.
#'
#' @return
#' @export
coleo_return_required_tables <- function(camp_type) {

  full_tbl <- coleo_get_required_tables()

  # test that camp_type is valid choice
  assertthat::assert_that(camp_type %in% names(full_tbl)[-1],
                          msg = "Not one of the database campaign type")

  tbls <- full_tbl[full_tbl[,camp_type]==1, "table"][[1]]

  return(tbls)
}
