## Cleanup des espèces
## By Steve Vissault

# ## Load libs
# library(tidyr)
# library(dplyr)
# library(taxize)
# library(magrittr)
#
# ## Base species db
# base_sp <- jsonlite::fromJSON(readLines("./refSpecies.json"))
#
# ## Vegetation
# vg <- data.frame(vernacular_fr=readRDS("sp_veg.rds"),stringsAsFactors=FALSE)
#
# ## Matches
# vg_matches <- sapply(vg[,1],x = base_sp$vernacular_fr, agrep)
#
# ## Pactch pour vérifier les
# is.empty <- function(x, mode=NULL){
#     if (is.null(mode)) mode <- class(x)
#     identical(vector(mode,1),c(x,vector(class(x),1)))
# }
#
# logic_test_match <- sapply(vg_matches,is.empty)
# vg_wo_entry <- names(logic_test_match[which(logic_test_match)])
#
#
# ## Obtenir l'information taxonomique depuis un nom commun FR
# vascan_lookup <- function(search) {
#
#   ress <- "http://data.canadensys.net/vascan/api/0.1/search.json"
#
#   if(length(search) == 1L){
#
#     return(httr::GET(ress, query = list(q = search)))
#
#   } else {
#     responses <- list()
#
#     for (s in 1:length(search)) responses[[s]] <- httr::GET(ress, query = list(q = search[s]))
#
#     return(responses)
#
#   }
# }
#
# vascan_matches <- lapply(vascan_lookup(vg_wo_entry),function(x) return(httr::content(x)$results[[1]]$matches[[1]]$canonicalName))
# vascan_matches <- unlist(lapply(vascan_matches, function(x) ifelse(is.null(x), return(NA), return(x))))
#
# ## On va chercher le TSN avec taxize
# library(taxize)
# resp <- list()
# for(i in 1:length(vascan_matches)) resp[[i]] <- ifelse(is.na(vascan_matches[i]),NA,get_tsn(vascan_matches[i]))
#
# ## On rassemble l'information
# vg_sp <- data.frame(genus=stringr::word(vascan_matches,1),species=stringr::word(vascan_matches,2),vernacular_fr=vg_wo_entry, tsn=as.numeric(unlist(resp)),stringsAsFactors= F)
#
# ## On fusionne l'info actuelle dans la DB avec les nouvelles espèces provenant des inventaires de la végétation
# all_sp <- bind_rows(base_sp,vg_sp)
# # Simple check if tsn est pas dupliqué
# all_sp <- all_sp[-which(duplicated(all_sp$tsn) & !is.na(all_sp$tsn)),]
# # save
# #write.csv2(all_sp,file="./all_sp.csv",row.names=FALSE)
#
# ## après quelques modifs manuels (aller chercher les TSN manquants sur ITIS), on vérifie encore la présence de doublons
# all_sp <- read.csv2("./all_sp_manuel_modif_SV.csv",stringsAsFactors=F)
# all_sp <- all_sp[-which(duplicated(all_sp$tsn) & !is.na(all_sp$tsn)),]
# # On récupère un genus et species pour les unités taxonomiques suivantes
# all_sp_wo_taxa <- subset(all_sp, !is.na(tsn) & is.na(species))
# all_sp <- all_sp[-which(!is.na(all_sp$tsn) & is.na(all_sp$species)),]
#
# for(i in 1:nrow(all_sp_wo_taxa)) all_sp_wo_taxa[i,'genus'] <- classification(all_sp_wo_taxa[i,'tsn'],db="itis")[[1]][11,1]
# for(i in 1:nrow(all_sp_wo_taxa)) all_sp_wo_taxa[i,'species'] <- stringr::word(classification(all_sp_wo_taxa[i,'tsn'],db="itis")[[1]][12,1],2)
#
# # On fusionne les deux tableaux
# all_sp <- dplyr::bind_rows(all_sp,all_sp_wo_taxa)
# all_sp$vernacular_fr <- tolower(all_sp$vernacular_fr)
#
# write.csv2(all_sp,file="./all_sp_final.csv",row.names=FALSE)

## Conversion du fichier final en JSON pour injection
# all_sp <- read.csv2("./all_sp_final_modif_SV.csv",stringsAsFactors=F)
# writeLines(jsonlite::toJSON(all_sp,auto_unbox=FALSE),con=file("./refSpecies.json"))

# ## Load libs
library(tidyr)
library(dplyr)
library(taxize)
library(magrittr)
#
# ## Base species db
base_sp <- jsonlite::fromJSON(readLines("./refSpecies.json"))
