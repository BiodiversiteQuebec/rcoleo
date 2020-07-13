###################################
####### PREP POST sur sites #######
###################################
sheet <- "odonate"

nms <- names(read_excel("./extdata/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date|Heure", nms, ignore.case = TRUE), "date", "guess")

## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./extdata/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet,col_types = ct)[-1,]

## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- str_replace_all(names(df)," ", "_")

## On sélectionne les colonnes d'intérêts
sites <- unique(select(df,cell_id=No_de_référence_de_la_cellule,site_code=No_de_référence_du_site,type=Type_de_milieu,opened_at="Date_d'inventaire",lat=Latitude, lon=Longitude))
sites$closed_at <- sites$opened_at

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

## TODO: Améliorer la fonction de POST - s'asssurer que le site n'éxite pas déjà
responses <- post_sites(sites_ls)

###################################
####### PREP POST sur campaign ####
###################################

library(reshape2)

campaigns <- unique(select(
  df,
  site_code=No_de_référence_du_site,
  opened_at="Date_d'inventaire",
  Heure_début,
  Heure_fin,
  ## ENV
  "Temp._(celsius)",
  Vent,
  Ciel
))

campaigns$type <- "odonates"
campaigns$closed_at <- campaigns$opened_at

# Transforme en list
campaigns_ls <- apply(campaigns,1,as.list)


techs <- unique(select(
  df,
  site_code = No_de_référence_du_site,
  opened_at="Date_d'inventaire",
  Observateur=Nom_observateur_1
))

techs$closed_at <- techs$opened_at


# On ajoute les technicians
campaigns_ls <- lapply(campaigns_ls, function(x){
  x$technicians <- as.list(techs[which(techs$site_code == x$site_code & as.character(techs$opened_at) == x$opened_at & as.character(techs$closed_at) == x$closed_at),]$Observateur)
  return(x)
})


# Ajout de l'effort d'échantillonnage
campaigns_ls <- lapply(campaigns_ls, function(x) {
  x$efforts <- list(
    list(samp_surf = "70685.83", samp_surf_unit = "m2", time_finish=as.character(strsplit(x$Heure_fin," ")[[1]][2]), time_start=as.character(strsplit(x$Heure_début," ")[[1]][2])
  ))
  return(x)
})

# Ajout de l'environments
campaigns_ls <- lapply(campaigns_ls, function(x) {
  x$environment <- list(
    list(sky = tolower(x$Ciel), wind = tolower(x$Vent), temp_c=x[['Temp._(celsius)']])
  )
  return(x)
})

##### POST Campaigns
responses <- post_campaigns(campaigns_ls)

##### TODO
#######################################
####### PREP POST sur observations ####
#######################################

# On selectionne les colonnes que l'on a besoin pour les Observations
obs <- select(df,site_code=No_de_référence_du_site,opened_at="Date_d'inventaire",espece="Esp._Odonate",value=nombre)
obs$type <- "odonates"
obs$closed_at <- obs$opened_at
obs$date_obs <- obs$opened_at

obs <- subset(obs, !is.na(espece))
substr(obs$espece, 1, 1) <- toupper(substr(obs$espece, 1, 1))

# # # On uniformise la taxo
# species <- select(obs, espece)
# names(species) <- "name"
# species$rank <- "espèce"
# species$vernacular_fr <- "NA"
# species[str_detect(species$name, "sp.$"),"rank"] <- "genre"
# # On essaye d'aller chercher les noms latins
# library(rvest)
# odonates_ls <-
# read_html("http://entomofaune.qc.ca/entomofaune/odonates/Liste_especes.html") %>%
# html_nodes("table") %>%
# .[[2]] %>%
#  html_table(header=TRUE, trim=TRUE)
# #
# names(odonates_ls) <- str_replace_all(names(odonates_ls), " ", "_")
# odonates_ls <- subset(odonates_ls, !is.na(Répartition))
# odonates_ls$Nom_scientifique <- str_replace_all(odonates_ls$Nom_scientifique, "[\r]" , " ")
# odonates_ls$Nom_scientifique <- str_replace_all(odonates_ls$Nom_scientifique, "[\n]" , "")
# odonates_ls$tsn <- taxize::get_tsn(odonates_ls$Nom_scientifique)
# write.csv2(odonates_ls,file="./vignettes/import/code_sp/odonates.csv",  row.names=FALSE)
# odonates_ls <- read.csv2("./vignettes/import/code_sp/odonates.csv",stringsAsFactors=FALSE)
# base_sp <- jsonlite::fromJSON(readLines("./vignettes/import/code_sp/refSpecies.json"))
# base_sp <- rbind(base_sp,odonates_ls)
# writeLines(jsonlite::toJSON(base_sp,auto_unbox=FALSE),con=file("./vignettes/import/code_sp/refSpecies.json"))

# On regarde si toutes les especes sont dans la table de reference
obs$espece <- str_replace_all(obs$espece, "Calopterys", "Calopteryx")
obs$espece <- str_replace_all(obs$espece, "Leste ", "Lestes ")
obs$espece <- str_replace_all(obs$espece, " sp.$", "")
obs$espece <- str_replace_all(obs$espece, "Inconnu", "inconnu")

responses <- get_species(name=obs$espece)
obs$sp_id <- unlist(lapply(responses, function(x)
if(is.null(x[[1]]$body$name)){ return(NA) } else {return(x[[1]]$body$name)}
))

## On prépare la structure pour l'injection des données
obs <- as.data.frame(obs)

injection_obs <- list()

for(i in 1:nrow(obs)){
  injection_obs[[i]] <- list(
    date_obs = obs[i,"opened_at"],
    is_valid = "true",
    campaign_info = list(
      site_code = obs[i,"site_code"],
      closed_at = obs[i,"closed_at"],
      opened_at = obs[i,"opened_at"],
      type = "odonates"
    ),
    obs_species = list(
      taxa_name = obs[i,"sp_id"],
      variable = "abondance",
      value = obs[i,"value"]
    )
  )
}

responses <- post_obs(injection_obs)
