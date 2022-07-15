
# tips for working with this table
# use datapasta::fiddle() to fix formatting after an update

#' Table des tables requises par type de campagne
#'
#'
#' @return a tbl_df showing which tables are required for which injection efforts.
coleo_get_required_tables <- function(){

tibble::tribble(
                       ~table, ~`végétation`, ~`végétation_transect`, ~sol, ~acoustique, ~`phénologie`, ~`mammifères`, ~`papilionidés`, ~odonates, ~insectes_sol, ~ADNe, ~zooplancton, ~`température_eau`, ~`température_sol`, ~`marais_profondeur_température`, ~`décomposition_sol`, ~`benthos`,
                      "cells",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                      "sites",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                  "campaigns",             0,                      0,    1,           1,             0,           0,               1,         1,             1,     0,            1,                  0,                  0,                              0,                        0,          1,
                    "efforts",             0,                      0,    0,           1,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          1,
               "environments",             0,                      0,    0,           0,             0,           0,               1,         1,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                    "devices",             0,                      0,    0,           1,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                      "lures",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                      "traps",             0,                      0,    0,           0,             0,           0,               0,         0,             1,     0,            0,                  0,                  0,                              0,                        0,          0,
                  "landmarks",             0,                      0,    1,           1,             0,           0,               1,         1,             1,     0,            1,                  0,                  0,                              0,                        0,          0,
                    "samples",             0,                      0,    0,           0,             0,           0,               0,         0,             1,     0,            0,                  0,                  0,                              0,                        0,          0,
               "thermographs",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
               "observations",             0,                      0,    1,           1,             0,           0,               1,         1,             1,     0,            1,                  0,                  0,                              0,                        0,          1,
                "ref_species",             0,                      0,    0,           1,             0,           0,               1,         1,             1,     0,            1,                  0,                  0,                              0,                        0,          1,
                "obs_species",             0,                      0,    0,           1,             0,           0,               1,         1,             1,     0,            1,                  0,                  0,                              0,                        0,          1,
                 "attributes",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
     "obs_soil_decomposition",             0,                      0,    1,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                   "obs_edna",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                   "obs_soil",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
      "obs_temperature_depth",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                      "media",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0,
                  "obs_media",             0,                      0,    0,           0,             0,           0,               0,         0,             0,     0,            0,                  0,                  0,                              0,                        0,          0
     )
}
