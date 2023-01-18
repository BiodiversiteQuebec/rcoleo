#' Trouver les colonnes, leur classe et les valeurs admissibles pour un type de campagne donné
#'
#' @param campaign_type un type de campagne
#' @param required.columns FALSE. Si TRUE, retourne seulement les colonnes requises pour procéder à l'injection
#'
#' @export
#'
coleo_return_cols <- function(campaign_type, required.columns = FALSE) {

  #-------------------------------------------------------------------------------
  # Vérifier que campaign_type est un choix valide
  #-------------------------------------------------------------------------------
  all_camp_types <- coleo2_request_general("rpc/get_enum_values",
    'enum_type' = "enum_campaigns_type", response_as_df = TRUE) |>
    unlist(use.names = FALSE)
  assertthat::assert_that(campaign_type %in% all_camp_types,
                          msg = "Entrez un type de campagne valide")
  #-------------------------------------------------------------------------------
  # Tables requises pour un type de campagne
  #-------------------------------------------------------------------------------
  req_tbls <- coleo_return_required_tables(campaign_type)
  #-------------------------------------------------------------------------------
  # Champs, classe des données et valeurs acceptées pour req_tbls
  #-------------------------------------------------------------------------------
  # Initialiser les objects pour sauver les infos
  table <- c()
  noms_de_champs <- c()
  classe <- c()
  colonne_requise <- c()
  valeurs_acceptees <- list()
  # Sauver les noms de colonne pour chaque table
  for(tbl in req_tbls){
    # Get columns from table
    cols_df <- coleo_get_column_names(tbl = tbl)

    # Si required.columns = TRUE, conserver seulement les colonnes requises
    if(required.columns) cols_df <- subset(cols_df, is_nullable == "NO")

    # Identifier les colonnes requises
    cols_df$is_nullable[cols_df$is_nullable == "NO"] <- TRUE
    cols_df$is_nullable[cols_df$is_nullable == "YES"] <- FALSE

    # Get values from enum columns
    values_df <- lapply(cols_df$udt_name, function(col) {
      if(grepl("enum", col)) {
        resp_enum <- coleo_get_enum_values(enum_col_name = col)
      }else NA_character_
    })

    # Assemble cols
    table <- c(table, rep(tbl, nrow(cols_df)))
    noms_de_champs <- c(noms_de_champs, cols_df$column_name)
    classe <- c(classe, cols_df$data_type)
    colonne_requise <- c(colonne_requise, cols_df$is_nullable)
    valeurs_acceptees <- c(valeurs_acceptees, values_df)
  }
  #-------------------------------------------------------------------------------
  # Assembler l'info dans un df
  #-------------------------------------------------------------------------------
  df <- as.data.frame(tibble::tibble(table = table,
                                     noms_de_champs = noms_de_champs,
                                     colonne_requise = colonne_requise,
                                     classe = classe,
                                     valeurs_acceptees = valeurs_acceptees))
  #-------------------------------------------------------------------------------
  # site_code est requis pour l'injection
  #-------------------------------------------------------------------------------
  site_code_row <- data.frame(table = "sites",
                              noms_de_champs = "site_code",
                              colonne_requise = "TRUE",
                              classe = "character",
                              valeurs_acceptees = NA_character_)
  df <- rbind(df,site_code_row)
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
                                 df$classe == "time without time zone")
  df$classe[change_to_character] <- "character"

  # ARRAY -> list
  change_to_list <- which(df$classe == "ARRAY")
  df$classe[change_to_list] <- "list"

  # Environments_ wind and environments_sky are integers
  sky_and_wind <- which(df$noms_de_champs == "sky" | df$noms_de_champs == "wind")
  df$classe[sky_and_wind] <- "integer"

  # geom -> lat / lon (character())
  geom_cols <- which(df$noms_de_champs == "geom")
  if("geom" %in% df$noms_de_champs) {
    for(i in seq_along(geom_cols)) {
      lat_row <- data.frame(table = df$table[geom_cols[i]],
                            noms_de_champs = "lat",
                            colonne_requise = "TRUE",
                            classe = "numeric",
                            valeurs_acceptees = NA_character_)
      df <- rbind(df, lat_row)
      lon_row <- data.frame(table = df$table[geom_cols[i]],
                            noms_de_champs = "lon",
                            colonne_requise = "TRUE",
                            classe = "numeric",
                            valeurs_acceptees = NA_character_)
      df <- rbind(df, lon_row)
    }
    df <- df[-geom_cols,]
  }

  ## double precision -> numeric()
  change_to_numeric <- which(df$classe == "double precision")
  df$classe[change_to_numeric] <- "numeric"

  ## Boolean -> logical
  change_to_logical <- which(df$classe == "boolean")
  df$classe[change_to_logical] <- "logical"

  #-------------------------------------------------------------------------------
  # Enlever les colonnes qui sont générées automatiquement lors de l'injection
  #-------------------------------------------------------------------------------
  # Remove id. To be added when prepping data for injection
  no_id <- which(!grepl("_id", df$noms_de_champs, fixed = TRUE) & df$noms_de_champs != "id")
  df <- df[no_id,]
  # Remove uuid. To be added when prepping data for injection
  no_uuid <- which(!grepl("uuid", df$noms_de_champs, fixed = TRUE) & df$noms_de_champs != "uuid")
  df <- df[no_uuid,]
  # Remove "created_at" et "updated_at"
  row_to_keep <- which(!grepl("created_at", df$noms_de_champs, fixed = TRUE) & df$noms_de_champs != "updated_at")
  df <- df[row_to_keep,]
  #-------------------------------------------------------------------------------
  # Définir les noms de colonnes à utiliser
  #-------------------------------------------------------------------------------
  ## Adjust names so that they are table_champ (whatch out for lat/lon!)
  df$noms_de_colonnes <- coleo_make_df_column_names(df$table, df$noms_de_champs)
  #-------------------------------------------------------------------------------
  # Nettoyer df
  #-------------------------------------------------------------------------------
  df <- df[,c("noms_de_colonnes","colonne_requise","classe","valeurs_acceptees")]

  return(tibble::as_tibble(df))
}
