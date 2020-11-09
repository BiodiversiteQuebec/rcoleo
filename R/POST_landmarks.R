#' Publication d'un landmark dans l'API de coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `landmarks` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_landmarks <- function (data)
{
  responses <- list()
  status_code <- NULL
  class(responses) <- "coleoPostResp"
  endpoint <- endpoints()$landmarks

  for (i in 1:length(data)) {
    responses[[i]] <- rcoleo::post_gen(endpoint, data[[i]])
    status_code <- c(status_code, responses[[i]]$response$status_code)
  }

  if(all(status_code == 201)){
    print("Good job ! Toutes les insertions ont été crées dans COLEO")
  }else{
    print("Oups... un problème est survenu")
    print(status_code)
  }
  return(responses)

}
