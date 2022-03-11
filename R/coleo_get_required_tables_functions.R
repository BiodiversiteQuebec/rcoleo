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
#' @param required_only boolean. Si FALSE, retourne toutes les colonnes admissibles pour le type de campagne. Autrement, retourne que les colonnes requises pour une injection valide
#'
#' @return
#' @export
coleo_return_cols <- function(campaign_type, required_only = TRUE) {

  # Tables requises pour un type de campagne
  req_tbls <- coleo_return_required_tables(campaign_type)

  resp_cols <- coleo_request_general(table = "campaigns", endpoint = "table_columns")
  cols_df <- purrr::map_dfr(httr2::resp_body_json(resp_cols), as.data.frame)
  cols_df

  # useful as validation after the renaming, to confirm that there are NO injections not in the possible column names


  resp_enum <- coleo_request_general(enum = "enum_sites_type", endpoint = "enum_options")
  httr2::resp_body_json(resp_enum) |> purrr::flatten() |> purrr::flatten_chr()

  # tricky part will be getting the enum thing

  cols_df
}
