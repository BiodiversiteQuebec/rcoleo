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



#' Retourne un vecteur contenant les noms valides de campagnes incluant les indicateurs de télédétection
#'
#'
#' @return vecteur de charactères contenant tous les types de capagnes valides
#' @export
#'
coleo_return_valid_campaigns <- function(){
  campaigns <- c(coleo_get_enum_values("enum_campaigns_type"), coleo_get_enum_values("enum_remote_sensing_indicators_name"))

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
#' @param camp_type un type de campagne valide.
#' @param col_names un vecteur de charactères contenant les noms des colonnes d'un jeu de données.
#'
#' @return Un vecteur contenant les noms des tables requises pour le type de campagne donné.
#'
#' @export
coleo_return_required_tables <- function(camp_type, col_names = NULL) {

  full_tbl <- coleo_get_required_tables()

  # test that camp_type is present among coleo_get_required_tables.R
  assertthat::assert_that(camp_type %in% names(full_tbl)[-1],
                          msg = "Cette campagne ne fait pas partie des choix possibles de coleo_get_required_tables.R")

  tbls <- full_tbl[full_tbl[,camp_type]==1, "table"][[1]]

  # If col_names is not NULL, check if the tables are required
  if (!is.null(col_names)) {
    if (!any(grepl("efforts", col_names))) {
      tables <- tables[!tables %in% "observations_efforts_lookup"]
    }
    if (!any(grepl("landmarks", col_names))) {
      tables <- tables[!tables %in% "landmarks"]
      tables <- tables[!tables %in% "observations_landmarks_lookup"]
    }
  }

  return(tbls)
}


#' Retourne le 'campaign_type' d'un jeu de données pour tous types d'inventaires.
#' 
#' @param data un jeu de données
#' 
#' @return le 'campaign_type' du jeu de données
#' 
#' @export
#' 
coleo_return_campaign_type <- function(data) {
  # Check for the presence of required columns
  if ("campaigns_type" %in% colnames(data)) {
    campaign_type <- unique(data$campaigns_type)
  } else if ("remote_sensing_indicators_name" %in% colnames(data)) {
    campaign_type <- unique(data$remote_sensing_indicators_name)
  } else {
    return(NULL)
  }
 
  return(campaign_type)
}
