#' Téléverse l'ensemble des médias appartenant à une campagnes
#'
#' @param x vector of campaign ids
#' @inheritParams get_gen
#' @return
#' Sauvegarde les images/sons attachés aux campagnes à une emplacement défini par l'utilisateur
#' @examples
#' \dontrun{
#' list_medias(x = c(101,104))
#' #Par exemple, on veut obtenir toutes les photos prises dans le cadre d'une campagne sur
#' # le site 141_108_F01 et portant sur les mammifères
#' get_medias(list_medias(x = c(101,104)))
#' }
#' @export


list_medias <- function(x, ...){

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoGetResp"

  endpoint <- endpoints()$media

  for(r in 1:length(x)){
    responses[[r]] <- get_gen(endpoint, query = list(campaign_id = x[r]), ...)
  }

  media <- as.data.frame(responses)

  # On regarde si la campagne contient des médias
  if(nrow(media) == 0){
    stop("Aucun m\u00e9dia attach\u00e9 \u00e0 ces campagnes")
  }

  media$uri <- paste0("/media/",media$type,"/",media$uuid,"/original")

  class(media) <- c("tbl_df", "tbl", "data.frame", "listMedias")

  return(media)
}

#' Télécharge l'ensemble des médias appartenant à une campagnes
#'
#' @param media object of class `listMedia`
#' @param output_dir `character` contenant le chemin d'accès vers le répetoire d'écriture des médias
#' @param verbose `logical` mode verbose
#' @inheritParams list_medias
#' @return
#' Sauvegarde les images/sons attachés aux campagnes à une emplacement définit par l'utilisateur
#' @examples
#' \dontrun{
#' #Par exemple, on veut obtenir toutes les photos prises dans le cadre d'une campagne
#' # sur le site 141_108_F01 et portant sur les mammiferes
#' get_medias(site_code="141_108_F01", type="mammif\u00e9res")
#' }
#' @export

get_medias <- function(media, output_dir = "./media", verbose = TRUE, ...) {


  token <- ifelse(is.na(bearer()), list(...)$token, bearer())

  stopifnot("listMedias" %in% class(media))

  # On créer le répertoire de sortie, s'il n'existe pas
  dir.create(output_dir)
  cat("T\u00e9l\u00e9versement en cours... \n")
  cat("0 sur", nrow(media), "\r")
  # Mieux gérer la progress bar
  # Vérifier si bug dans les chemins avec Windows
  for(m in 1:nrow(media)){
    if(verbose){
      if((m %% 100) == 0) {
        cat(m, "sur", nrow(media), "\r")
      }
    }
    httr::GET(
    url = paste0(server(),media$uri[m]),
    config = httr::add_headers(Authorization = paste("Bearer", ifelse(is.na(bearer()),token,bearer()))),
    ua, httr::write_disk(file.path(output_dir,paste0(media$name[m],media$og_extention[m])), overwrite=TRUE)
    )
  }
}

