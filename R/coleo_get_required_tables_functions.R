# functions that work with coleo_get_required_tables()

#' Trouver les tables requises pour un type de capamgne donné
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


#' Trouver les colonnes requises, leur classe et les valeurs admissibles pour un type de campagne donné
#'
#' @param campaign_type un type de campagne valide.
#' @param required_only boolean. Si FALSE, retourne toutes les colonnes admissibles pour le type de campagne. Autrement, retourne que les colonnes requises pour une injection valide
#'
#' @return
#' @export
coleo_return_df_cols <- function(campaign_type, required_only = TRUE) {

  # Tables requises pour un type de campagne
  req_tbls <- coleo_return_required_tables(campaign_type)

  # Noms de colonnes requis pour ces tables
  # Ajouter manuellement site_code
  req_columns <- c(
    unlist(
      sapply(req_tbls,
             ifelse(required_only,
                    coleo_get_required_name_table,
                    coleo_get_name_table_column))),
    "site_code")

  # Classes des colonnes requises
  tbl <- coleo_get_name_table()
  req_class <- lapply(req_columns, function(x) {
    ## Table "campaign" peut demander la colonne campaign_id
    ## Est demandé par la fonction coleo_get_name_table_column()
    ifelse(x == "campaign_id", NA_character_,
           tbl$required_class[[which(tbl$input_column==x)]])
    })

  # Valeurs des colonnes
  req_values <- lapply(req_columns, function(x) {
    # Table "campaign" peut demander la colonne campaign_id
    ## Est demandé par la fonction coleo_get_name_table_column()
    ifelse(x == "campaign_id", NA_character_,
           list(tbl$legal_values[[which(tbl$input_column==x)]]))
    })

  # Assembler l'info dans un df
  req_cols <- as.data.frame(tibble::tibble(noms_colonnes = req_columns,
                                           classe = req_class,
                                           valeurs_acceptées = req_values))

  View(req_cols)
}


#' Trouver les colonnes requises, leur classe et les valeurs admissibles pour un type de campagne donné
#'
#' @param campaign_type un type de campagne valide.
#'
#' @return
#' @export
coleo_return_cols <- function(campaign_type) {

  #-------------------------------------------------------------------------------
  # Tables requises pour un type de campagne
  #-------------------------------------------------------------------------------
  req_tbls <- coleo_return_required_tables(campaign_type)
  #-------------------------------------------------------------------------------
  # Champs, classe des données et valeurs acceptées requises pour req_tbls
  #-------------------------------------------------------------------------------
  # Initialiser les objects pour sauver les infos
  table <- c()
  noms_de_champs <- c()
  classe <- c()
  valeurs_acceptées <- list()
  # Sauver les noms de colonne pour chaque table
  for(tbl in req_tbls){
    # Get columns from table
    resp_cols <- coleo_request_general(table = tbl, endpoint = "table_columns")
    cols_df <- purrr::map_dfr(httr2::resp_body_json(resp_cols), as.data.frame)

    # Get values from enum columns
    values_df <- lapply(cols_df$udt_name, function(col) {
      if(grepl("enum", col)) {
        resp_enum <- coleo_request_general(enum = col, endpoint = "enum_options")
        httr2::resp_body_json(resp_enum) |> purrr::flatten() |> purrr::flatten_chr()
      }else NA_character_
    })

    # Assemble cols
    table <- c(table, rep(tbl, nrow(cols_df)))
    noms_de_champs <- c(noms_de_champs, cols_df$column_name)
    classe <- c(classe, cols_df$data_type)
    valeurs_acceptées <- c(valeurs_acceptées, values_df)
  }
  #-------------------------------------------------------------------------------
  # Assembler l'info dans un df
  #-------------------------------------------------------------------------------
  df <- as.data.frame(tibble::tibble(table = table,
                                     noms_de_champs = noms_de_champs,
                                     classe = classe,
                                     valeurs_acceptées = valeurs_acceptées))
  #-------------------------------------------------------------------------------
  # Special column class pour injection
  #-------------------------------------------------------------------------------
  # USER-DEFINED, character varying, text, date & timestamp with time zone -> character()
  change_to_character <- which(df$classe == "USER-DEFINED" |
                                 df$classe == "character varying" |
                                 df$classe == "text" |
                                 df$classe == "date" |
                                 df$classe == "timestamp" |
                                 df$classe == "timestamp with time zone" |
                                 df$classe == "time without time zone" |
                                 df$classe == "ARRAY")
  df$classe[change_to_character] <- "character"

  # geom -> lat / lon (character())
  geom_cols <- which(df$noms_de_champs == "geom")
  if("geom" %in% df$noms_de_champs) {
    for(i in seq_along(geom_cols)) {
      lat_row <- data.frame(table = df$table[geom_cols[i]],
                            noms_de_champs = "lat",
                               classe = "numeric",
                               valeurs_acceptées = NA_character_)
      df <- rbind(df, lat_row)
      lon_row <- data.frame(table = df$table[geom_cols[i]],
                            noms_de_champs = "lon",
                            classe = "numeric",
                            valeurs_acceptées = NA_character_)
      df <- rbind(df, lon_row)
    }
    df <- df[-geom_cols,]
  }

  ## double precision -> numeric()
  change_to_numeric <- which(df$classe == "double precision")
  df$classe[change_to_numeric] <- "character"

  ## Boolean -> logical
  change_to_numeric <- which(df$classe == "double precision")
  df$classe[df$classe == "boolean"] <- "logical"

  ## Remove id. To be added when prepping data for injection
  id_to_remove <- which(grepl("_id", df$noms_de_champs, fixed = TRUE) | df$noms_de_champs == "id")
  df <- df[-id_to_remove,]
  #-------------------------------------------------------------------------------
  # Définir les noms de colonnes à utiliser
  #-------------------------------------------------------------------------------
  ## Adjust names so that they are table_champ (whatch out for lat/lon!)
  df$noms_de_colonnes <- paste(df$table, df$noms_de_champs, sep = "_")
  #-------------------------------------------------------------------------------
  # Nettoyer df
  #-------------------------------------------------------------------------------
  df <- df[,c("noms_de_colonnes","classe","valeurs_acceptées")]

  return(df)
}
