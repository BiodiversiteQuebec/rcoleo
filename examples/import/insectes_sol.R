

###################################
####### PREP POST sur sites #######
###################################
sheet <- "Carabes_Installation"

library(readxl)
library(tidyverse)
library(geojsonio)

nms <- names(read_excel("./examples/import/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date|Heure", nms, ignore.case = TRUE), "date", "guess")

## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./examples/import/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet,col_types = ct)[-1,]

## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- stringr::str_replace_all(names(df)," ", "_")

## On sélectionne les colonnes d'intérêts
sites <- unique(select(df,cell_id=No_de_référence_de_la_cellule,site_code=No_de_référence_de_site,type_de_milieu=Type_de_milieu,opened_at="Date_d'installation",latA="latitude_P-A", lonA="Longiture_P-A",latB="latitude_P-B",lonB="Longiture_P-B", latC="latitude_P-C", lonC="Longiture_P-C"))


sites
# what is this latA, lonA, business?! why are there three


## Remplacer les barres de soulignement par des tirets
sites$site_code <- str_replace_all(sites$site_code,"-", "_")

## On Transforme les lat et long en numérique
sites$latA <- type.convert(sites$latA)
sites$lonA <- type.convert(sites$lonA)
sites$latB <- type.convert(sites$latB)
sites$lonB <- type.convert(sites$lonB)
sites$latC <- type.convert(sites$latC)
sites$lonC <- type.convert(sites$lonC)

## Pour ce type campaign pas le choix de calculer le centroide de la distributions des traps
sites$lat <- rowMeans(sites[,c("latA","latB","latC")])
sites$lon <- rowMeans(sites[,c("lonA","lonB","lonC")])

## On séléctionne les champs d'interêts
sites <- select(sites,cell_id,site_code,type=type_de_milieu,opened_at,lat,lon)

## Transformer en liste pour injection
sites_ls <- apply(sites,1,as.list)

# Creer geom points
geom <- apply(sites,1, function(x){
  if(!any(is.na(x["lat"]),is.na(x["lon"]))){
  return(geojson_list(as.numeric(c(x["lon"],x["lat"])))$features[[1]]$geometry)
} else {
  return(NA)
}})


sites_purr_ls <- purrr::transpose(sites)

all.equal(sites_ls, sites_purr_ls) # ok so there is some variation

library(purrr)
geom %>% map(is.list)

crs_ls <- list(type="name",properties=list(name="EPSG:4326"))

geom %>% map_if(is.list, append, list(crs = crs_ls))

# Fusionner les deux listes (geomations + sites)
for(i in 1:length(sites_ls)){
  sites_ls[[i]]$geom <- geom[i][[1]]
  if(is.list(sites_ls[[i]]$geom)){
    sites_ls[[i]]$geom$crs <- list(type="name",properties=list(name="EPSG:4326"))
  }
}

sites_ls

sites_ls_test <- sites_ls[1]

str(sites_ls_test)

sites_ls_test[[1]]["opened_at"] <- "2020-07-10"

sites_ls_test[[1]]["site_code"] <- "135_104_Q99"

sites_ls[[1]] %>% dput

library(rcoleo)
#responses <- post_sites(sites_ls_test)

responses
###################################
##### PREP POST sur campaigns #####
###################################

library(reshape2)

# On séléctionne les champs: site_code, type, techs, opened_at, closed_at
campaigns <- unique(select(
  df,
  site_code = `No de référence de site`,
  `Nom observateur 2...7`,
  `Nom observateur 1...6`,
  `Nom observateur 1...9`,
  `Nom observateur 1...9`,
  `Nom observateur 2...10`,
  opened_at=`Date d'installation`,
  closed_at="Date de récolte 2 et retrait",
  "No piège A" ,
  "No piège B" ,
  "No piège C" ,
  latA="latitude P-A",
  lonA="Longiture P-A",
  latB="latitude P-B",
  lonB="Longiture P-B",
  latC="latitude P-C",
  lonC="Longiture P-C"
))

campaigns$type <- "insectes_sol"
campaigns$key <- rownames(campaigns)

## On prépare le jeux de données pour le POST
campaigns <- melt(data=campaigns, id.vars=c("site_code","type","opened_at","closed_at","key", "No piège A","No piège B","No piège C","latA", "lonA","latB", "lonB","latC", "lonC"),na.rm=TRUE)
campaigns <- select(campaigns, -variable)
names(campaigns)[ncol(campaigns)] <- "tech"
campaigns <- melt(data=campaigns, id.vars=c("site_code","type","opened_at","closed_at","key","tech","latA", "lonA","latB", "lonB","latC", "lonC"),na.rm=TRUE)
names(campaigns)[ncol(campaigns)] <- "traps"

# On récupère l'ID du piège
campaigns$variable <- str_remove_all(campaigns$variable,"No piège ")

# On attribut les bonnes coordonnées
campaigns$lat_trap <- as.numeric(apply(campaigns, 1, function(x){
  x[paste0("lat",x["variable"])]
}))
campaigns$lon_trap <- as.numeric(apply(campaigns, 1, function(x){
  x[paste0("lon",x["variable"])]
}))

campaigns$variable[1]

# On retire ce que l'on a plus besoins
campaigns <- unique(select(campaigns,-latA,-lonA,-latB,-lonB,-latC,-lonC,-variable))

###
techs <- unique(select(campaigns, key, tech))
techs <- subset(techs, tech != "NA" & !is.na(tech) )
traps <- unique(select(campaigns, site_code, opened_at, closed_at, trap_code=traps, lat_trap, lon_trap))
campaigns <- unique(select(campaigns,-traps, -tech))
###

## On le transforme en liste pour l'injection finale
campaigns_ls <- apply(campaigns,1,as.list)
names(campaigns_ls) <- NULL

# On loop sur la liste pour ajouter les techniciens
for(l in 1:length(campaigns_ls)){
  k <- as.numeric(campaigns_ls[[l]]$key)
  tech_add <- unique(subset(techs, key == k))
  campaigns_ls[[l]]$technicians <- as.list(tech_add$tech)
}

campaigns_ls[[1]] %>% dput


#responses <- post_campaigns(campaigns_ls)

#### Traps + landmarks (GPS) ===========
# On prépare le jeux de données pour insertion dans la table Traps


traps <- unique(select(campaigns, site_code, opened_at, closed_at, trap_code=traps, lat_trap, lon_trap))

library(rcoleo)

new_camps <- get_campaigns(
  site_code=traps$site_code,
  opened_at=traps$opened_at,
  closed_at=traps$closed_at,
  type=rep("insectes_sol",nrow(traps)))

new_camp_df <- new_camps[[1]] %>% map_df("body")

new_camp_df$id


## On le transforme en liste pour l'injection finale
traps$campaign_id <- as.data.frame(new_camp_df)$id

traps_ls <- apply(traps,1,as.list)
names(traps_ls) <- NULL

# Creer geom points
traps_ls <- lapply(traps_ls, function(x) {

  if(any(!is.na(x$lat_trap),!is.na(x$lon_trap))){
    geom <- geojson_list(as.numeric(c(x$lat_trap,x$lon_trap)))$features[[1]]$geometry
  } else {
    geom <- NULL
  }

  if(any(x$closed_at == "NA" | is.na(x$closed_at))){ x$closed_at <- NULL }

  x$landmarks <- list(list(campaign_id=x$campaign_id,geom = geom))

  return(x)

})


traps_ls[[1]] %>% dput

# responses <- post_traps(traps_ls)

##### Ajout des échantillons dans la table
library(reshape2)

names(df)

samples <- unique(select(df,
  site_code="No de référence de site",
  opened_at="Date d'installation",
  piège_A="No piège A",
  piège_B="No piège B",
  piège_C="No piège C",
  date_1 ="Date de récolte 1",
  date_2 ="Date de récolte 2 et retrait",
  ech_A_1="No échantillon 1",
  ech_B_1="No échantillon 2",
  ech_C_1="No échantillon 3",
  ech_A_2="No échantillon 4",
  ech_B_2="No échantillon 5",
  ech_C_2="No échantillon 6"))

samples$closed_at <- samples$date_2

samples <- melt(samples, id.vars=c("site_code","opened_at","closed_at","date_1","date_2","piège_A","piège_B","piège_C"),na.rm=TRUE)
samples <- rename(samples,code=variable, sample_code = value)
samples$date_samp <- NA
samples$trap_code <- NA

for(i in 1:nrow(samples)){
  str_code <- strsplit(as.character(samples[i,"code"]),"[_]")[[1]][2:3]
  samples[i,"date_samp"] <- as.character(samples[i,paste("date",str_code[2],sep="_")])
}

for(i in 1:nrow(samples)){
  str_code <- strsplit(as.character(samples[i,"code"]),"[_]")[[1]][2:3]
  samples[i,"trap_code"] <- samples[i,paste("piège",str_code[1],sep="_")]
}

## the above programatically generates column names
glimpse(df)
# i.e. it picks which of no piege C for example is required.. oof hard to read and think about!

samples <- unique(select(samples,site_code,opened_at,closed_at,sample_code,date_samp,trap_code))
# samples$year <- format(as.Date(samples$date_samp),"%Y")
# year_samp <- rep(paste0(samples$year,"%"),nrow(samples))

# On transforme en list pour l'injection
samples_ls <- apply(samples,1,as.list)
names(samples_ls) <- NULL

samples_ls[[5]] %>% dput

# responses <- post_samples(samples_ls)

########## RESHAPING #######
#### Observations + ObsSpecies

sheet <- "Carabes_Tri"

nms <- names(read_excel("./examples/import/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date|Heure", nms, ignore.case = TRUE), "date", "guess")

## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./examples/import/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet,col_types = ct)

## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- str_replace_all(names(df)," ", "_")

## On fait un melt sur la feuille
df_melted <- reshape2::melt(df,id.vars=c("Nom_de_la_cellule","No_de_référence_de_la_cellule","No_réf._du_site","Récolte","Date_de_récolte","No_de_piège","No_échantillon"))
df_melted$variable <- unlist(lapply(strsplit(as.character(df_melted$variable),"\\_"), function(x) x[[2]]))
names(df_melted)[8] <- "groupe_taxonomique"
names(df_melted)[9] <- "abondance"
df <- df_melted

## On prend les colonnes que l'on a besoin
## On retire les carabidées car ils sont identifiés à l'espèce
df <- filter(df, groupe_taxonomique !=  "carabes")
df$groupe_taxonomique <- str_replace_all(df$groupe_taxonomique, "autres", "inconnu")
df$groupe_taxonomique <- str_replace_all(df$groupe_taxonomique, "de", "mollusque")

## On prépare les données pour injection autre qu'à l'échelle de l'espèce
obs <- unique(select(df,date_obs=Date_de_récolte,sample_code=No_échantillon, site_code="No_réf._du_site", vernacular_fr=groupe_taxonomique, value=abondance, trap_code=No_de_piège))
obs$type <- "insectes_sol"


# search with steve list
## On regarde si toutes les taxons sont présent dans la BD
# resp_taxon <- get_species(vernacular_fr=obs$vernacular_fr)
sp_id <- unlist(lapply(resp_taxon, function(x) return(x[[1]]$body[,c("name")])))

## On récupère les identifiants unique d'échantillon
resp_samples <- get_samples(sample_code=obs$sample_code)
sample_id <- unlist(lapply(resp_samples, function(x) return(x[[1]]$body[,c("id")])))
trap_id <- unlist(lapply(resp_samples, function(x) return(x[[1]]$body[,c("trap_id")])))

## On récupère le code de campaign à partir des traps
resp_traps <- list()
for(i in 1:length(trap_id)){
  resp_traps[[i]] <- httr::content(httr::GET(url=paste0(server(),"/api/v1/traps/",trap_id[i]), config = httr::add_headers(`Content-type` = "application/json",Authorization = paste("Bearer", bearer())),ua), simplify = TRUE)
}

campaign_id <- unlist(lapply(resp_traps, function(x) return(x$campaign_id)))

## Valider que les dates d'échantillonages présent dans traps
## correspondent bien à ceux présent à l'observation
obs$campaign_id <- campaign_id
obs$trap_id <- trap_id
obs$sample_id <- sample_id
obs$sp_id <- sp_id

obs <- as.data.frame(obs)

injection_obs <- list()

for(i in 1:nrow(obs)){
  injection_obs[[i]] <- list(
    date_obs = obs[i,"date_obs"],
    is_valid = "true",
    campaign_id = obs[i,"campaign_id"],
    sample_id = obs[i, "sample_id"],
    obs_species = list(
      taxa_name = obs[i,"sp_id"],
      variable = "abondance",
      value = obs[i,"value"]
    )
  )
}

# responses <- post_obs(injection_obs)

## Injection des carabes identifie à l'espèce en laboratoire

sheet <- "Carabidae_ID"

nms <- names(read_excel("./examples/import/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date|Heure", nms, ignore.case = TRUE), "date", "guess")

## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./examples/import/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet,col_types = ct)[-1,]

glimpse(df)
## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- str_replace_all(names(df)," ", "_")

glimpse(df)


### On normalise la taxonomie
df_taxa <- data.frame(famille=df$Famille,species=paste(df$Genre, df$Espèce),stringsAsFactors=FALSE)
df_taxa$taxa_lookup <- apply(df_taxa,1,function(x) ifelse(x["species"] == "NA sp", return(x["famille"]), return(x["species"])))
df_taxa[which(df_taxa$taxa_lookup == "Platynus decentis (decens)"),"taxa_lookup"] <- "Platynus decentis"

## On ajoute la nouvelle colonne de taxonomie
df$taxa <- df_taxa$taxa_lookup

glimpse(df)
# on n'a plus besoin faire ça

## On sélectionne les champs dont on a besoin pour l'injection
## On prépare les données pour injection autre qu'à l'échelle de l'espèce
obs <- unique(select(df,
                     date_obs=Date_récolte,
                     sample_code=Échantillon,
                     site_code="No._Site",
                     taxa,
                     value=Nombre))
glimpse(obs)
obs <- as.data.frame(obs)
obs$type <- "insectes_sol"

head(obs)
## On regarde si toutes les taxons sont présent dans la BD
resp_taxon <- get_species(name=obs$taxa)

sp_id <- unlist(lapply(resp_taxon, function(x) return(x[[1]]$body[,c("name")])))
cbind(obs$taxa, sp_id)

obs$sp_id <- sp_id

## On récupère les identifiants unique d'échantillon
resp_samples <- get_samples(sample_code=obs$sample_code)
obs$sample_id <- unlist(lapply(resp_samples, function(x) return(x[[1]]$body[,c("id")])))
obs$trap_id <- unlist(lapply(resp_samples, function(x) return(x[[1]]$body[,c("trap_id")])))

glimpse(obs)

## On récupère le code de campaign à partir des traps
resp_traps <- list()
for(i in 1:length(obs$trap_id)){
  resp_traps[[i]] <- httr::content(httr::GET(url=paste0(rcoleo:::server(),"/api/v1/traps/",obs$trap_id[i]), config = httr::add_headers(`Content-type` = "application/json",Authorization = paste("Bearer", rcoleo:::bearer())),httr::user_agent("rcoleo")), simplify = TRUE)
}

## where are the campaign ids??!
resp_traps[[1]]

obs$campaign_id <- unlist(lapply(resp_traps, function(x) return(x$campaign_id)))
obs <- as.data.frame(obs)
glimpse(obs)
tail(obs)

injection_obs <- list()

for(i in 1:nrow(obs)){
  injection_obs[[i]] <- list(
    date_obs = obs[i,"date_obs"],
    is_valid = "true",
    campaign_id = obs[i,"campaign_id"],
    sample_id = obs[i, "sample_id"],
    obs_species = list(
      taxa_name = obs[i,"sp_id"],
      variable = "abondance",
      value = obs[i,"value"]
    )
  )
}

obs

dput(injection_obs[[1]])

obs[1,]

# responses <- post_obs(injection_obs)

# dput(injection_obs[1])

test_obs <- list(list(
  date_obs = "2020-12-25",
  is_valid = "true",
  campaign_id = 45,
  sample_id = 5,
  obs_species = list(taxa_name = "Platynus decentis",
                     variable = "abondance",
                     value = 400)))

post_obs(test_obs)
