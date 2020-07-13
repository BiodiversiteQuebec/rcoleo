# Lectures du fichier avec les métadonnées
md <- readRDS("../media-cleanup/mam_md_wt_cp.rds")

library(readxl)
sheet <- "Feuil1"
nms <- names(read_excel("./extdata/V1_Mammifère_Campagne_2016-2017.xlsx",sheet=sheet))
## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date", nms, ignore.case = TRUE), "date", "guess")
## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./extdata/V1_Mammifère_Campagne_2016-2017.xlsx",sheet=sheet,col_types = ct,trim_ws=TRUE)[-1,]
names(df) <- stringr::str_replace_all(names(df)," ","_")

df$key <- rownames(df)
df$site_code <- ifelse(df$Type_de_milieu == "forestier", paste0(df$No_de_référence_de_la_cellule,"_F01"),paste0(df$No_de_référence_de_la_cellule,"_T01"))
df$type <- "mammifères"

###################################
#### PREP POST sur sites ######
###################################

## On séléctionne les champs d'interêts
sites <- select(df,cell_id="No_de_référence_de_la_cellule",site_code,type=Type_de_milieu,opened_at="Date_d'installation", closed_at = "Date_de_retrait",lat="Latitude_A-C",lon="Longitude_A-C")

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
                        site_code = "site_code",
                        milieu = "Type_de_milieu",
                        type = "type",
                        cam_code = "No_caméra",
                        opened_at = "Date_d'installation",
                        closed_at = "Date_de_retrait"
                      ))


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
              site_code,
              type,
              opened_at = "Date_d'installation",
              closed_at = "Date_de_retrait",
              cam_code = "No_caméra",
              cam_h_cm = "Hauteur_caméra_C-A_(cm)",
              sd_card_codes = "No_carte_C-A"
          ))

devices[which(devices$cam_h_cm == "nd"),"cam_h_cm"] <- NA

devices_ls <- apply(devices, 1, as.list)
devices_ls <- lapply(devices_ls, function(x){
  if(is.na(x$cam_h_cm)){ x$cam_h_cm <- NULL }
  return(x)
})

devices_ls <- lapply(devices_ls, function(x){
  x$sd_card_codes <- as.list(x$sd_card_codes)
  return(x)
})

responses <- post_devices(devices_ls)

###################################
#### PREP POST sur lures ######
###################################

names(df)
lures_appats <- unique(select(df,
              site_code,
              type,
              opened_at = "Date_d'installation",
              closed_at = "Date_de_retrait",
              "Appat_1" = "Appât_1",
              "Appat_2",
              "Appat_3",
              "Appat_4",
              "Appat_5",
          ))

lures_appat_melt <- reshape2::melt(lures_appats, id.vars=c("site_code","type","opened_at","closed_at"))
names(lures_appat_melt)[6] <- "lure"
lures_appat_melt$key <- stringr::str_extract(lures_appat_melt$variable,"[:digit:]")
lures_appat_melt <- lures_appat_melt[,-c(5)]

lures_dates <- unique(select(df,
              site_code,
              type,
              opened_at = "Date_d'installation",
              closed_at = "Date_de_retrait",
              "Date_appat_2" = "Date_appatage_2",
              "Date_appat_3" = "Date_appatage_23",
              "Date_appat_4" = "Date_appatage4",
              "Date_appat_5" = "Date_appatage_5"
          ))
lures_dates$Date_appat_1 <- lures_dates$opened_at

lures_dates_melt <- reshape2::melt(lures_dates, id.vars=c("site_code","type","opened_at","closed_at"))

names(lures_dates_melt)[6] <- "date_lure"
lures_dates_melt$key <- stringr::str_extract(lures_dates_melt$variable,"[:digit:]")
lures_dates_melt <- lures_dates_melt[-which(is.na(lures_dates_melt$date_lure)),]
lures_dates_melt <- lures_dates_melt[,-c(5)]

lures <- merge(lures_appat_melt,lures_dates_melt, by=c("site_code","type","opened_at","closed_at","key"))
lures <- select(lures,"site_code","type","opened_at","closed_at","lure",installed_at="date_lure")
lures$type <- "mammifères"
lures_ls <- apply(lures, 1, as.list)

responses <- post_lures(lures_ls)

###################################
#### PREP POST sur landmarks ######
###################################

## MISSING

###################################
#### PREP POST sur médias ######
###################################

devtools::load_all(".")
library(httr)
mam_md <- readRDS("../media-cleanup/inject_mam.rds")

mam_md$url <- paste0("http://coleo-media:3002/upload/campaign/",mam_md$camp_id)
mam_md$filename <- unlist(lapply(strsplit(mam_md$media,"[/]"), function(x) return(x[8])))
mam_md <- mam_md[-which(duplicated(mam_md$filename)),]

responses <- list()

for(i in 1:nrow(mam_md)){
  responses[[i]] <- httr::POST(mam_md$url[i], body = list(type="image",media=upload_file(mam_md$media[i])),
  config = httr::add_headers(`Content-type` = "multipart/form-data", Authorization = paste("Bearer",
    bearer())))
}



