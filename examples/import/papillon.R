###################################
####### PREP POST sur sites #######
###################################

sheet <- "papillon"

nms <- names(read_excel("./extdata/V3_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date|Heure", nms, ignore.case = TRUE), "date", "guess")

## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./extdata/V3_CompilationDonnées_2016-2018.xlsx",sheet="papillon",col_types = ct)[-1,]

## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- str_replace_all(names(df)," ", "_")


## On remplace ERRORS
df$No_de_référence_de_la_cellule <- str_replace_all(df$No_de_référence_de_la_cellule,"137_108", "137_107")
df$No_de_référence_de_la_cellule <- str_replace_all(df$No_de_référence_de_la_cellule,"142_112", "142_111")

## On réplique la date pour la date de fermeture
df <- rename(df, opened_at = "Date_d'inventaire")
df$closed_at <- df$opened_at

## On sélectionne les colonnes d'intérêts
sites <- select(df,No_de_référence_de_la_cellule,No_de_référence_du_site,Type_de_milieu,opened_at,closed_at,Latitude,Longitude)

## On renomme les colonnes
names(sites) <- c("cell_id","site_code","type","opened_at","closed_at","lat","lon")

## Garder les entrées unique par site
sites <- unique(sites)

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

## TODO: Améliorer la fonction de POST - s'asssurer que le site n'éxite pas déjà

responses <- post_sites(sites_ls)


###################################
####### PREP POST sur campaign ####
###################################

library(reshape2)

campaigns <- unique(select(
  df,
  No_de_référence_du_site,
  opened_at,
  closed_at,
  Heure_début,
  Heure_fin,
  ## ENV
  "Temp._(celsius)",
  Vent,
  Ciel,
  Remarque
))

campaigns <- rename(campaigns,site_code=No_de_référence_du_site)
campaigns$type <- "papilionidés"

## Remplacer les barres de soulignement par des tirets
campaigns$site_code <- str_replace_all(campaigns$site_code,"-", "_")

# Transforme en list
campaigns_ls <- apply(campaigns,1,as.list)


techs <- unique(select(
  df,
  site_code = No_de_référence_du_site,
  opened_at,
  closed_at,
  Observateur
))

# On ajoute les technicians
campaigns_ls <- lapply(campaigns_ls, function(x){
  x$technicians <- as.list(techs[which(techs$site_code == x$site_code & as.character(techs$opened_at) == x$opened_at & as.character(techs$closed_at) == x$closed_at),]$Observateur)
  return(x)
})


# Ajout de l'effort d'échantillonnage
campaigns_ls <- lapply(campaigns_ls, function(x) {

  samp_surf <- ifelse(format(as.Date(x$opened_at),"%Y") == 2016, 2000, 5000)

  x$efforts <- list(
    list(samp_surf = samp_surf, samp_surf_unit = "m2", time_finish=as.character(strsplit(x$Heure_fin," ")[[1]][2]), time_start=as.character(strsplit(x$Heure_début," ")[[1]][2])
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

#######################################
####### PREP POST sur observations ####
#######################################

# On selectionne les colonnes que l'on a besoin pour les Observations
obs <- select(df,site_code=No_de_référence_du_site,opened_at,closed_at,espece="Esp._Papillon",value=nombre)


# On regarde si toutes les especes sont dans la table de reference
sp <- obs$espece
responses <- get_species(vernacular_fr=sp)
obs$sp_id <- unlist(lapply(responses, function(x) return(x[[1]]$body$name)))


## On ajoute l'attributs d'abondance dans la table de reference
resp <- post_attributes(data=list(list(variable="abondance",description="nombre d'individus observés", unit="")))
attr_id <- resp[[1]]$body$variable

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
      type = "papilionidés"
    ),
    obs_species = list(
      taxa_name = obs[i,"sp_id"],
      variable = attr_id,
      value = obs[i,"value"]
    )
  )
}

responses <- post_obs(injection_obs)
