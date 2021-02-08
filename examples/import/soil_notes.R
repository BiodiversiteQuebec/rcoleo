# notes on upload file



# one element of sites list:
# telecharge +
# geo        +

list(cell_id = "135_104", site_code = "135_104_F01", type = "forestier",
     opened_at = "2016-05-20", lat = "46.79409", lon = "-72.30314",
     geom = list(type = "Point", coordinates = c(-72.30314, 46.79409
     ), crs = list(type = "name", properties = list(name = "EPSG:4326"))))

# one element of list campaigns
# tele -
# geo  -

list(site_code = "135_104_F01", type = "insectes_sol", opened_at = "2016-05-20",
     closed_at = "2016-07-12", key = "1", lat_trap = "46.79415",
     lon_trap = "-72.30296", technicians = list("Caroline Dubé",
                                                "Pierre-Alexis Drolet"))

## traps --
# tele + (campaign_id)
# geo +
list(site_code = "135_104_F01", opened_at = "2016-05-20", closed_at = "2016-07-12",
     trap_code = "MP-05", lat_trap = "46.79415", lon_trap = "-72.30296",
     campaign_id = "41",
     landmarks = list(
       list(campaign_id = "41",
            geom = list(type = "Point",
                        coordinates = c(46.79415, -72.30296))
            )
       )
     )

## samples
# tele -
# geom -
list(site_code = "148_101_F01", opened_at = "2016-05-17", closed_at = "2016-07-21",
     sample_code = "2016-0001", date_samp = "2016-06-21", trap_code = "MP-04")


## observations

# must get species
resp_taxon <- get_species(vernacular_fr=obs$vernacular_fr)
# telecharge: species, samples, traps, campaign


list(date_obs = structure(1466467200, class = c("POSIXct", "POSIXt"
), tzone = "UTC"),
is_valid = "true",
campaign_id = 45L,
sample_id = 5L,
obs_species = list(taxa_name = "Platynus decentis",
                   variable = "abondance",
                   value = 4))




V7_CompilationDonnees_2016_2019_insectes_tri <- readr::read_csv("V7_CompilationDonnees_2016-2019_insectes_tri.csv")
View(V7_CompilationDonnees_2016_2019_insectes_tri)

test2 <- get_sites(site_code = "103_87_L01")
rrr <- get_gen(endpoint, query = list(site_code = site_code[id]), ...)

tibble::as_tibble(jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = TRUE))

