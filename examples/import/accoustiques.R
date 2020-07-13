###################################
####### PREP POST sur sites #######
###################################
sheet <- "Inv. acoustique"

nms <- names(read_excel("./extdata/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date|Heure", nms, ignore.case = TRUE), "date", "guess")

## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./extdata/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet,col_types = ct)[-1,]

## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- str_replace_all(names(df)," ", "_")

## On sélectionne les colonnes d'intérêts
sites <- unique(select(df,cell_id=No_de_référence_de_la_cellule,site_code=No_référence_du_site,type=Type_de_milieu,opened_at="Date_d'installation",lat="Latitude", lon="Longitude"))

###################################
#### PREP POST sur sites ######
###################################

## Remplacer les barres de soulignement par des tirets
sites$site_code <- str_replace_all(sites$site_code,"-", "_")


## Transformer en liste pour injection
sites_ls <- apply(sites,1,as.list)

# Creer geom points
geom <- apply(sites,1, function(x){
  if(!any(is.na(x["lat"]),is.na(x["lon"]))){
  return(geojson_list(as.numeric(c(x["lon"],x["lat"])))$features[[1]]$geometry)
} else {
  return(NA)
}})

# Fusionner les deux listes (geomations + sites)
for(i in 1:length(sites_ls)){
  sites_ls[[i]]$geom <- geom[i][[1]]
  if(is.list(sites_ls[[i]]$geom)){
    sites_ls[[i]]$geom$crs <- list(type="name",properties=list(name="EPSG:4326"))
  }
}

responses <- post_sites(sites_ls)

### On vérifie pour les sites
library(tidyr)
library(dplyr)
campaigns <- unique(select(df,
                        cell_name = "Nom_de_la_cellule",
                        cell_code = "No_de_référence_de_la_cellule",
                        site_code = "No_référence_du_site",
                        opened_at = "Date_d'installation",
                        closed_at = "Date_de_retrait"
                      ))

campaigns$type <- "acoustique"

campaigns_ls <- apply(campaigns, 1, as.list)

###################################
#### PREP POST sur campagnes ######
###################################

# on prepare les campagnes pour injection
# 1. Techniciens
for(i in 1:length(campaigns_ls)){
  techs <- as.list(c(df[i,"Nom_observateur_1"],df[i,"Nom_observateur_2"]))
  names(techs) <- NULL
  campaigns_ls[[i]]$technicians <- as.list(techs[!is.na(techs)])
}

# on fusionne les deux listes
responses <- post_campaigns(campaigns_ls)

###################################
#### PREP POST sur devices ######
###################################

devices <- unique(select(df,
              site_code = "No_référence_du_site",
              opened_at = "Date_d'installation",
              closed_at = "Date_de_retrait",
              mic_logger_code = "No_appareil",
              mic_ultra_code = "No_micro_ultrason",
              mic_h_cm_ultra = "Hauteur_micro_ultrason",
              mic_acc_code = "No_micro_acoustic",
              mic_h_cm_acc = "Hauteur_micro_acoustic_(cm)",
              mic_orientation = "Orientation_des_micros",
              sd_card_codes_1 = "No_carte_1",
              sd_card_codes_2 = "No_carte_2"
          ))

devices$type <- "acoustique"
devices$opened_at <- as.character(devices$opened_at)
devices$closed_at <- as.character(devices$closed_at)

dev <- strsplit(devices$mic_orientation, "-")
devices$mic_orientation <- sapply(dev, function(x){
      if(!is.na(x)) tolower(paste(substring(x, 1, 1), collapse = ""))
  })


devices_ls <- apply(devices, 1, as.list)

#### PROB ICI
devices_ls <- lapply(devices_ls, function(x){
  for(n in names(x)) x[n] <- ifelse(is.na(x[n]), list(NULL), x[n])
  return(x)
})

devices_ls <- lapply(devices_ls, function(x){
  x$sd_card_codes <- list()
  x$sd_card_codes <- as.list(x$sd_card_codes_1, x$sd_card_codes_2)
  return(x)
})

devices_ls[[27]] <- devices_ls[[27]][-which(sapply(devices_ls[[27]], is.null))]


responses <- post_devices(devices_ls)

###################################
#### PREP POST sur landmarks ######
###################################

## MISSING

###################################
#### PREP POST sur médias ######
###################################

devtools::load_all(".")
library(httr)
acc_md <- readRDS("../media-cleanup/acc_with_cp.rds")

test_files <- list.files("./extdata/media/acoustique",recursive=TRUE)
str_match(acc_md$new_filename,test_files)

mam_md$url <- paste0("http://coleo-media:3002/upload/campaign/",mam_md$camp_id)
mam_md$filename <- unlist(lapply(strsplit(mam_md$media,"[/]"), function(x) return(x[8])))
mam_md <- mam_md[-which(duplicated(mam_md$filename)),]

responses <- list()

for(i in 1:nrow(mam_md)){
  responses[[i]] <- httr::POST(mam_md$url[i], body = list(type="acoustique",media=upload_file(mam_md$media[i])),
  config = httr::add_headers(`Content-type` = "multipart/form-data", Authorization = paste("Bearer",
    bearer())))
}



