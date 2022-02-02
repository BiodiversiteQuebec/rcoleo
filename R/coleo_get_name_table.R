
# tips for working with this table
# use datapasta::fiddle() to fix formatting after an update

# table = the table in the database
# input_column = the column name in the input
# db_column = the column name to be injected into the database
# required_class = the class-checking function required
# legal_values = a vector of what values it is allowed to have.
coleo_get_name_table <- function(){
  tibble::tribble(
                     ~table,         ~input_column,            ~db_column, ~required,  ~required_class,                                                                          ~legal_values,
                "campaigns",             "site_id",             "site_id", TRUE,      is.numeric,                                                                                     NA,
                "campaigns",      "camp_opened_at",           "opened_at", FALSE,     is.character,                                                                                     NA,
                "campaigns",      "camp_closed_at",           "closed_at", FALSE,     is.character,                                                                                     NA,
                "campaigns",           "camp_type",                "type", TRUE,     is.character, c('végétation', 'végétation_transect', 'sol', 'acoustique', 'phénologie', 'mammifères',
                                                                                               'papilionidés', 'odonates', 'insectes_sol', 'ADNe','zooplancton', 'température_eau',
                                                                                               'température_sol', 'marais_profondeur_température'),
                "campaigns",        "technician_1",        "technician_1", FALSE,     is.character,                                                                                     NA,
                "campaigns",        "technician_2",        "technician_2", FALSE,     is.character,                                                                                     NA,
                "campaigns",          "camp_notes",               "notes", FALSE,     is.character,                                                                                     NA,
             "observations",            "date_obs",            "date_obs", TRUE,     is.character,                                                                                     NA,
             "observations", "time_obs", "time_obs", FALSE, is.character, NA
             "observations",               "depth",               "depth", FALSE,     is.character,                                                                                     NA,
             "observations",           "obs_notes",               "notes",     is.character,                                                                                     NA,
             "observations",            "is_valid",            "is_valid",       is.logical,                                                                                     NA,
             "observations",         "campaign_id",         "campaign_id",     is.character,                                                                                     NA,
    "obs_soil_decompsition",         "decomp_type",                "type",     is.character,                                                                                     NA,
    "obs_soil_decompsition",         "decomp_type",                "type",     is.character,                                                                                     NA,
    "obs_soil_decompsition",              "bag_no",              "bag_no",     is.character,                                                                                     NA,
    "obs_soil_decompsition",        "start_weight",        "start_weight",       is.numeric,                                                                                     NA,
    "obs_soil_decompsition", "end_weight_with_bag", "end_weight_with_bag",       is.numeric,                                                                                     NA,
    "obs_soil_decompsition",             "shading",             "shading",       is.numeric,                                                                                     NA,
    "obs_soil_decompsition",        "human_impact",        "human_impact",       is.numeric,                                                                                     NA,
                    "sites",             "cell_id",             "cell_id",     is.character,                                                                                     NA,
                    "sites", "off_station_code_id", "off_station_code_id",     is.character,                                                                                     NA,
                    "sites",           "site_code",           "site_code",     is.character,                                                                                     NA,
                    "sites",           "site_name",           "site_name",     is.character,                                                                                     NA,
                    "sites",           "site_type",                "type",     is.character, c("lac", "rivière", "forestier", "marais", "marais côtier", "toundrique", "tourbière"),
                    "sites",      "site_opened_at",           "opened_at",     is.character,                                                                                     NA,
                    "sites",            "site_lat",                 "lat",       is.numeric,                                                                                     NA,
                    "sites",            "site_lon",                 "lon",       is.numeric,                                                                                     NA,
                    "sites",          "site_notes",               "notes",     is.character,                                                                                     NA,
                "landmarks",         "campaign_id",         "campaign_id",     is.character,                                                                                     NA,
                "landmarks",            "land_lat",                 "lat",       is.numeric,                                                                                     NA,
                "landmarks",            "land_lon",                 "lon",       is.numeric,                                                                                     NA,
                     "taxa",            "category",            "category",     is.character,                                                                                     NA,
                     "taxa",           "taxa_name",                "name",     is.character,                                                                                     NA,
                     "taxa",                "rank",                "rank",     is.character,         c('sous-embranchement', 'embranchement', 'sous-classe', 'classe', 'sous-ordre',
                                                                                                       'ordre', 'super-famille', 'famille', 'genre', 'espèce','sous-espèce',
                                                                                                       'variété', 'complexe','genre_hybride', 'espèce_hybride','variété_hybride',
                                                                                                       'sous-espèce_hybride'),
                     "taxa",            "category",            "category",     is.character,               c('poissons','plantes','oiseaux','amphibiens','arthropodes','mammifères',
                                                                                                             'reptiles','autres','mollusques'),
              "obs_species",           "taxa_name",           "taxa_name",     is.character,                                                                                     NA,
              "obs_species",            "variable",            "variable",     is.character,                                                                                     NA,
              "obs_species",      "observation_id",      "observation_id",       is.integer,                                                                                     NA,
              "obs_species",               "value",      "observation_id",       is.numeric,                                                                                     NA,
                    "traps",           "trap_code",           "trap_code",     is.character,                                                                                     NA,
                    "traps",         "campaign_id",         "campaign_id",     is.character,                                                                                     NA,
                    "traps",         "traps_notes",               "notes",     is.character,                                                                                     NA,
                  "samples",         "sample_code",         "sample_code",     is.character,                                                                                     NA,
                  "samples",             "trap_id",             "trap_id",       is.integer,                                                                                     NA
  )
}


coleo_return_valid_campaigns <- function(){
  full_tbl <- coleo_get_name_table()

  legal_vals <- subset(full_tbl, table == "campaigns" & input_column == "camp_type")[["legal_values"]][[1]]

  return(legal_vals)
}


#' Find the names of the campaigns required
#'
#' @param camp_type a legal type of campaign.
#'
#' @return
#' @export
#'
#' @examples
coleo_get_required_tables <- function(camp_type){

tibble::tribble(
                       ~names, ~végétation, ~végétation_transect, ~sol, ~acoustique, ~phénologie, ~mammifères, ~papilionidés, ~odonates, ~insectes_sol, ~ADNe, ~zooplancton, ~température_eau, ~température_sol, ~marais_profondeur_température,
                      "cells",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                      "sites",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                  "campaigns",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                    "efforts",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
               "environments",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                    "devices",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                      "lures",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                      "traps",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                  "landmarks",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                    "samples",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
               "thermographs",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
               "observations",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                "obs_species",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                 "attributes",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                "ref_species",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
     "obs_soil_decomposition",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                   "obs_edna",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                   "obs_soil",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
      "obs_temperature_depth",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                      "media",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0,
                  "obs_media",           0,                    0,    0,           0,           0,           0,             0,         0,             0,     0,            0,                0,                0,                              0
     )

}
