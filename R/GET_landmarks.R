#' Retourne les entrées pour les repères sur le terrain spécifique à une campagne d'inventaires

#' @inheritParams get_campaigns
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' \dontrun{
#' get_landmarks()
#' }
#' @export

get_landmarks <- function(site_code = NULL, opened_at = NULL, closed_at = NULL, type = NULL, ...) {


  token <- ifelse(is.na(bearer()), list(...)$token, bearer())

  endpoint <- endpoints()$landmarks

  # Preparation de l'objet de sortie
  responses <- list()

  # Si tous les arguments sont nuls
  if(all(is.null(site_code), is.null(opened_at), is.null(closed_at), is.null(type))){

    responses[[1]] <- get_gen(endpoint, ...)

  } else {

    # tests args to set iterator
    len <- max(c(length(site_code),length(opened_at),length(closed_at),length(type)))

    # Prep query
    for(r in 1:len){
      campaigns_ids <- as.data.frame(get_campaigns(site_code=site_code[r],
                    opened_at = opened_at[r],
                    closed_at = closed_at[r],
                    type = type[r]))$id
    }

    # On récupère les landmarks pour la campagne concernée
    for(i in 1:length(campaigns_ids)) responses[[i]] <- get_gen(endpoint, query=list(campaign_id=campaigns_ids[i]), ...)

    # On ajoute les informations sur la campagne
    responses <- lapply(responses, function(response){

        lapply(response, function(page){

          if(attributes(page$body)$n_records == 0) return(page)

          campaign_id <- unique(page$body$campaign_id)
          stopifnot(length(campaign_id) == 1)


          # Campagne info
          campaign_info <- httr::content(httr::GET(
            url = paste0(server(),"/api/v1/campaigns/",campaign_id),
            config = httr::add_headers(`Content-type` = "application/json",
                                       Authorization = paste("Bearer", ifelse(is.na(bearer()),
                                                                              token,bearer()))),
            ua),
            simplify = TRUE)
          # Code du site
          site_code <- httr::content(httr::GET(
            url=paste0(server(),"/api/v1/sites/",campaign_info$site_id),
            config = httr::add_headers(`Content-type` = "application/json",
                                       Authorization = paste("Bearer", ifelse(is.na(bearer()),token,bearer()))),ua),
            simplify = TRUE)$site_code

          # On prepare la sortie
          page$body$site_code <- site_code
          page$body$opened_at <- campaign_info$opened_at
          page$body$closed_at <- campaign_info$closed_at
          page$body$type <- campaign_info$type

          page$body <- dplyr::select(page$body,
            "id",
            "site_code",
            "opened_at",
            "closed_at",
            "type",
            "campaign_id",
            "axis",
            "trap_id",
            "lure_id",
            "device_id",
            "tree_code",
            "taxa_name",
            "dbh",
            "dbh_unit",
            "azimut",
            "distance",
            "distance_unit",
            "notes",
            "created_at",
            "updated_at"
          )

          return(page)

      })
    })
  }

  class(responses) <- "coleoGetResp"
  return(responses)

}
