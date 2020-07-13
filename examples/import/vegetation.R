###################################
####### PREP POST sur sites #######
###################################

sheet <- "Végétation"

nms <- names(read_excel("./extdata/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date", nms, ignore.case = TRUE), "date", "guess")


## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./extdata/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet,col_types = ct,trim_ws=TRUE)[-1,]

## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- str_replace_all(names(df)," ", "_")

sites <- select(df,No_de_référence_de_la_cellule,No_de_référence_du_site,Type_de_milieu,Date_inventaire_printanier,Date_inventaire_estival,No_borne_forestière,Latitude,Longitude)

## Configurer la date d'ouverture du site
sites$date_open <- ifelse(is.na(sites$Date_inventaire_printanier),as.character(sites$Date_inventaire_estival),as.character(sites$Date_inventaire_printanier))
sites$date_open <- as.Date(sites$date_open)

## Retirer les dates estivales et printannière
sites <- select(sites,-Date_inventaire_estival,-Date_inventaire_printanier)

names(sites) <- c("cell_id","site_code","type","off_station_code_id","lat","lon","opened_at")

## Garder les entrées unique par site
sites <- unique(sites)

## Remplacer les barres de soulignement par des tirets
sites$site_code <- str_replace_all(sites$site_code,"-", "_")

## CORRECTIONS
## Garder monit_prg_station_id juste pour les programmes externes
sites[str_detect(sites$off_station_code_id,"(\\d{3})-(\\d{3})"),"off_station_code_id"] <- NA

## Transformer en liste pour injection
sites_ls <- apply(sites,1,as.list)

# Creer geom points
geom <- apply(sites,1, function(x){
  if(!any(is.na(x["lat"]),is.na(x["lon"]))){
  return(geojson_list(as.numeric(c(x["lon"],x["lat"])))$features[[1]]$geometry)
} else {
  return(NA)
}})

# Fusionner les deux listes (locations + sites)
for(i in 1:length(sites_ls)){
  sites_ls[[i]]$geom <- geom[i][[1]]
  if(is.list(sites_ls[[i]]$geom)){
    sites_ls[[i]]$geom$crs <- list(type="name",properties=list(name="EPSG:4326"))
  }
}

##############################
####### POST sur sites #######
##############################
resp_sites <- post_sites(sites_ls)

###################################
####### PREP POST sur campaign ####
###################################

library(reshape2)

campaigns <- unique(select(df,No_de_référence_du_site,Date_inventaire_printanier,Date_inventaire_estival,Nom_observateur_1__1, Nom_observateur_2__1,Nom_observateur_1, Nom_observateur_2))
campaigns$type <- "végétation"
names(campaigns) <- c("site_code","opened_at","closed_at","Obs1","Obs2","Obs3","Obs4","type")
campaigns$key <- rownames(campaigns)

# S'il n'y pas de date de fermeture, alors on prend la date d'ouverture et inversement
campaigns[is.na(campaigns$closed_at),"closed_at"] <- campaigns[is.na(campaigns$closed_at),"opened_at"]
campaigns[is.na(campaigns$opened_at),"opened_at"] <- campaigns[is.na(campaigns$opened_at),"closed_at"]

## Remplacer les barres de soulignement par des tirets
campaigns$site_code <- str_replace_all(campaigns$site_code,"-", "_")

## Melt pour les observateurs
techs <- select(campaigns,key,Obs1,Obs2,Obs3,Obs4)
techs <- melt(techs,id.vars=c("key"),na.rm=TRUE)
techs <- techs[which(techs$value != "NA"),]
techs <- unique(select(techs, -variable))

# On enlève les champs Observateurs des campagnes
campaigns <- select(campaigns,-Obs1,-Obs2,-Obs3,-Obs4)

# Transforme en list
campaigns_ls <- apply(campaigns,1,as.list)

# On loop sur la liste pour ajouter les techniciens
for(l in 1:length(campaigns_ls)){
  k <- as.numeric(campaigns_ls[[l]]$key)
  tech_add <- unique(subset(techs, key == k))
  campaigns_ls[[l]]$technicians <- tech_add$value
}


# Ajout de l'effort d'échantillonnage
campaigns_ls <- lapply(campaigns_ls, function(x) {
  x$efforts <- list(
    list(stratum = "bryophytes", samp_surf = "1", samp_surf_unit = "m2"),
    list(stratum = "arbustes/herbacées", samp_surf = "100", samp_surf_unit = "m2"),
    list(stratum = "arbres", samp_surf = "400", samp_surf_unit = "m2")
  )
  return(x)
})

##### POST Campaigns
responses <- post_campaigns(campaigns_ls)


###################
# AJOUT LANDMARKs #
###################

# Ajout des landmarks
landmarks_A <- unique(df[,c("No_de_référence_du_site","Date_inventaire_printanier","Date_inventaire_estival", "No_de_l'arbre_repère_A_(AR-A)", "Essence", "DHP_(mm)_AR-A", "Azimut_(o)_AR-A", "Distance_AR-A_(cm)","Latitute_AR-A","Londitude_AR-A")])
landmarks_A$type <- "végétation"
names(landmarks_A) <- c("site_code","opened_at","closed_at", "tree_code","sp","dbh","azimut","distance","lat","lon","type")

# Ajout des landmarks
landmarks_B <- unique(df[,c("No_de_référence_du_site","Date_inventaire_printanier","Date_inventaire_estival", "No_de_l'arbre_repère_B_(AR-B)", "Essence__1", "DHP_(mm)_AR-B", "Azimut_(o)_AR-B", "Distance_AR-B_(cm)", "Latitute_AR-B", "Londitude_AR-B")])
landmarks_B$type <- "végétation"
names(landmarks_B) <- c("site_code","opened_at","closed_at", "tree_code","sp","dbh","azimut","distance","lat","lon","type")

# Binding sur les lignes
landmarks <- bind_rows(landmarks_A,landmarks_B)

# S'il n'y pas de date de fermeture, alors on prend la date d'ouverture et inversement
landmarks[is.na(landmarks$closed_at),"closed_at"] <- landmarks[is.na(landmarks$closed_at),"opened_at"]
landmarks[is.na(landmarks$opened_at),"opened_at"] <- landmarks[is.na(landmarks$opened_at),"closed_at"]

## Remplacer les barres de soulignement par des tirets
landmarks$site_code <- str_replace_all(landmarks$site_code,"-", "_")

## Retirer les campagnes qui ne disposent pas de landmarks
landmarks <- landmarks[-which(landmarks[,c(4:8)]=="NA" | is.na(landmarks[,c(4:8)])),]

# Clean taxonomy
landmarks$sp <- toupper(landmarks$sp)

# Créer la correspondance avec les codes
species_code <- data.frame(code=c("ES","SB","PU","EO","BG","BP","BJ"), vernacular_fr = c("Érable à sucre", "Sapin baumier", "Pruche de l'Est", "Érable rouge", "Bouleau gris", "Bouleau à papier", "Bouleau jaune"))
# TODO: Valider les correspondances, surtout pour prunus et érable rouge

species_code <- cbind(species_code,as.data.frame(get_species(vernacular_fr = species_code[,"vernacular_fr"])))

landmarks$taxa_name <- species_code[match(landmarks$sp,species_code$code),"name"]
landmarks$type <- "both"

## Final touch, prep geom et data
landmarks_ls <- apply(landmarks,1,as.list)

geom <- apply(landmarks,1, function(x){
  if(!any(is.na(x["lat"]),is.na(x["lon"]))){
  return(geojson_list(as.numeric(c(x["lon"],x["lat"])))$features[[1]]$geometry)
} else {
  return(NA)
}})

# Fusionner les deux listes (locations + sites)
for(i in 1:length(landmarks_ls)){
  landmarks_ls[[i]]$geom <- geom[i][[1]]

  if(is.list(landmarks_ls[[i]]$geom)){
    landmarks_ls[[i]]$geom$crs <- list(type="name",properties=list(name="EPSG:4326"))
  }
}

# Post landmarks
resp <- post_landmarks(landmarks_ls)

##################
## Observations ##
##################

obs <- select(df,No_de_référence_du_site,Date_inventaire_estival,Date_inventaire_printanier,Type_de_végétation,Espèce,"Recouvrement_(%)")

## Fix observations dates
obs$Date_obs <- obs$Date_inventaire_printanier
obs[which(is.na(obs$Date_obs)),"Date_obs"] <- obs[which(is.na(obs$Date_obs)),"Date_inventaire_estival"]

## Fix columnNames
names(obs) <- c("site_code","closed_at","opened_at","stratum","species","recouvrement","date_obs")

## Fix sur date ouverture et fermeture
# S'il n'y pas de date de fermeture, alors on prend la date d'ouverture et inversement
obs[is.na(obs$closed_at),"closed_at"] <- obs[is.na(obs$closed_at),"opened_at"]
obs[is.na(obs$opened_at),"opened_at"] <- obs[is.na(obs$opened_at),"closed_at"]

## species
# saveRDS(unique(obs$species),file="./vignettes/import/code_sp/sp_veg.rds")
# Toutes les espèces ont été intégré dans la table ref_species. Fichier de traitement dans ./import/code_sp/cleanup.r

## On récupère le code de l'espèce
resp <- get_species(vernacular_fr=obs$species)
sp_id <- unlist(lapply(resp,function(x) return(x[[1]]$body$name)))
rec <- unlist(lapply(resp, function(x) return(attributes(x[[1]]$body)$n_records)))

## Non REPRODUCTIBLE
obs$species[which(rec == 0)] <- c("pruche de l'est","canneberge commune","saule","frêne de pennsylvanie","amélanchier","pruche de l'est", "amélanchier","petit thé")

# On recommence avec les noms vernaculaires modifiés
resp <- get_species(vernacular_fr=obs$species)
sp_id <- unlist(lapply(resp,function(x) return(x[[1]]$body$name)))
rec <- unlist(lapply(resp, function(x) return(attributes(x[[1]]$body)$n_records)))

if(length(sp_id) == nrow(obs)) obs$sp_id <- sp_id

# On documente l'attribut de Recouvrement
resp <- post_attributes(data=list(list(variable="recouvrement",description="évaluation du recouvrement à l’intérieur de la placette", unit="%")))

# On set le id du attribute
variable <- "recouvrement"

obs <- as.data.frame(obs)
obs$site_code <- str_replace_all(obs$site_code,"-","_")
obs$recouvrement <- as.numeric(obs$recouvrement)

injection_obs <- list()

for(i in 1:nrow(obs)){
  injection_obs[[i]] <- list(
    date_obs = as.Date(obs[i,"date_obs"]),
    is_valid = "true",
    stratum = tolower(obs[i,"stratum"]),
    campaign_info = list(
      site_code = obs[i,"site_code"],
      closed_at = as.Date(obs[i,"closed_at"]),
      opened_at = as.Date(obs[i,"opened_at"]),
      type = "végétation"
    ),
    obs_species = list(
      taxa_name = obs[i,"sp_id"],
      variable = variable,
      value = obs[i,"recouvrement"]
    )
  )
}

responses <- post_obs(injection_obs)
