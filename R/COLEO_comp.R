#' Compare deux vecteurs et donne des détails si ils ne sont pas égaux
#'
#' @param x one thing to compare
#' @param y second thing to compare
#'
#' @return
#' un message
#' @examples
#' \dontrun{
#' # Vecteurs egaux
#' i <- 1:5
#' j <- 1:10
#'
#' COLEO_comp(i, j)
#'
#' # Vecteurs inegaux
#' i <- 1:13
#' j <- 1:10
#'
#' COLEO_comp(i, j)

#' }
#' @export
COLEO_comp <- function (x, y)
{
  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(is.vector(y))

  list_info <- NULL

  if (all(x %in% y) == TRUE) {

    message("Vous pouvez passer a l'etape suivante.")
  }
  else {
    for (i in 1:length(x)) {
      if (x[i] %in% y == FALSE) {
        list_info <- c(list_info, x[i])
      }
    }
    message(paste0("Coleo ne contient pas d'informations pour la donnee suivante:\n",
                 paste0(list_info, collapse = ", "), "\n",
                 ". Cliquez sur le bouton de la vignette correspondante dans le tutoriel d'injections de campagnes."))
  }
  return(list_info)
}
