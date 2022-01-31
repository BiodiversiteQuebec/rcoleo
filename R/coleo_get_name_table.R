# table = the table in the database
# input_column = the column name in the input
# db_column = the column name to be injected into the database
# required_class = the class-checking function required
# legal_values = a vector of what values it is allowed to have.
coleo_get_name_table <- function(){
  tibble::tribble(
    ~table,         ~input_column,            ~db_column,  ~required_class,                                                                          ~legal_values,
    "campaigns",             "site_id",             "site_id",       is.numeric,                                                                                     NA,
    "campaigns",      "camp_opened_at",           "opened_at",     is.character,                                                                                     NA,
    "campaigns",      "camp_closed_at",           "closed_at",     is.character,                                                                                     NA,
    "campaigns",           "camp_type",                "type",     is.character,       c('végétation', 'végétation_transect', 'sol', 'acoustique', 'phénologie', 'mammifères',
                                                                                         'papilionidés', 'odonates', 'insectes_sol', 'ADNe','zooplancton', 'température_eau',
                                                                                         'température_sol', 'marais_profondeur_température'),
    "campaigns",        "technician_1",        "technician_1",     is.character,                                                                                     NA,
    "campaigns",        "technician_2",        "technician_2",     is.character,                                                                                     NA,
    "observations",            "date_obs",            "date_obs",     is.character,                                                                                     NA,
    "observations",               "depth",               "depth",     is.character,                                                                                     NA,
    "observations",           "obs_notes",               "notes",     is.character,                                                                                     NA,
    "observations",            "is_valid",            "is_valid",       is.logical,                                                                                     NA,
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
    "taxa",                "rank",                "rank",     is.character,                                                                                     NA
  )

}
