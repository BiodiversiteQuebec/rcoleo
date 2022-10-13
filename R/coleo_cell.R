# data about cells needs to be validated and prepared in a certain way before
# being injected. These functions make those checks. coleo_cell_df will also
# return a data.frame for a specific cell

#' Converti les cellules du format sf (objet sf) en un un objet list.
#'
#' Les cellules doivent être injectées en tant que listes imbriquées. La
#' lecture des sites en R se fait généralement en utilisant sf::read_sf().
#' Cette fonction converti l'objet sf en une liste imbriquée via geojsonio.
#'
#' @param cell_geom La géométrie d'une cellule en tant qu'objet sf de type 'polygon'.
#'
#' @return Une liste imbriquée contenant les coordonnées du polygone.
#' @export
coleo_cell_geom_fmt <- function(cell_geom){
  # take a single sf class polygon and return the nested list suitable for injection

  # test if object is sf or not.
  if (!any("sfg" == class(cell_geom) | "sfc" == class(cell_geom))) stop("This function is meant to work on sf objects.")

  one_cell_list <- geojsonio::geojson_list(cell_geom)

  one_cell_list$coordinates <- list(
    purrr::set_names(split(one_cell_list$coordinates[[1]],
                           seq_len(nrow(one_cell_list$coordinates[[1]]))),
             NULL))

  attr(one_cell_list, which = c("class")) <- NULL
  attr(one_cell_list, which = c("from")) <- NULL

  return(one_cell_list)
}

# cell injections are done with coleo_inject_general
