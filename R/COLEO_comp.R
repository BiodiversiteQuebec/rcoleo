#' Compare deux vecteurs et donne des détails si ils ne sont pas égaux
#'
#' @return
#' un message
#' @examples
#' \dontrun{
#' # Vecteurs égaux
#' i <- 1:5
#' j <- 1:10
#'
#' COLEO_comp(i, j)
#'
#' # Vecteurs inégaux
#' i <- 1:13
#' j <- 1:10
#'
#' COLEO_comp(i, j)

#' }
#' @export
COLEO_comp <- function (x, y)
{
  if (all(x %in% y) == TRUE) {
    print("Vous pouvez passer à l'étape suivante.")
  }
  else {
    list_info <- NULL
    for (i in 1:length(x)) {
      if (x[i] %in% y == FALSE) {
        print(paste0("Coléo ne contient pas d'informations pour la donnée suivante:",
                     x[i],
                     ". Cliquez sur le bouton de la vignette correspondante dans le tutoriel d'injections de campagnes.",
                     collapse = ""))
        list_info <- c(list_info, x[i])
      }
    }
    return(list_info)
  }
}
