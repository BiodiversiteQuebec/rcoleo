#' Effectuer un diagnotic rapide de l'ensemble des appels sur l'API
#'
#' @param responses liste de réponse des fonctions de GET/POST
#' @export

diagnostic <- function(responses=responses){

  status <- list()

  if(class(responses) == "coleoPostResp"){
    for(r in seq_len(length(responses))){
        status[[length(status)+1]] <- as.data.frame(httr::http_status(responses[[r]]$response))
    }
  } else if(class(responses) == "coleoGetResp"){
    for(r in seq_len(length(responses))){
      for(q in seq_len(length(responses))){
          status[[length(status)+1]] <- as.data.frame(httr::http_status(responses[[r]][[q]]$response))
      }
    }
  }

  plyr::rbind.fill(status)

}
