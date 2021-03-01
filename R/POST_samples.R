#' Publication d'un échantillon dans l'API de coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `samples` de l'API de Coleo
#'
#' @param data une liste ou chacun des niveaux corresponds aux données attribuées au ednpoint.
#' @param ... httr options; arguments de la fonction `httr::POST()`
#'
#' @export

post_samples <- function(data, ...) {

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoPostResp"

  endpoint <- endpoints()$samples

  for (i in 1:length(data)) {

    # On retourne l'id unique pour la campagne à laquelle est rattaché les trappes
    # Le unlist c'est pour les pages, mais je sais que la réponse contient une seule page (match sur un code)
    campaign_id <- as.data.frame(get_campaigns(site_code=data[[i]]$site_code,
                                               opened_at=data[[i]]$opened_at,
                                               closed_at=data[[i]]$closed_at,
                                               type="insectes_sol"))$id
    stopifnot(length(campaign_id) == 1)
    data[[i]]$campaign_id <- as.numeric(campaign_id)

    # On obtient le code de la trappe
    trap_id <- as.data.frame(get_traps(campaign_id=data[[i]]$campaign_id,trap_code=data[[i]]$trap_code))$id
    stopifnot(length(trap_id) == 1)
    data[[i]]$trap_id <- trap_id

    responses[[i]] <- post_gen(endpoint, data[[i]], ...)
  }

  return(responses)
}
