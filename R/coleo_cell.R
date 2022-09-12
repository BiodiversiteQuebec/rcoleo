# data about cells needs to be validated and prepared in a certain way before
# being injected. These functions make those checks. coleo_cell_df will also
# return a data.frame for a specific cell

#' Convert cell from sf to list
#'
#' We need to inject a nested list structure into the database. When we read
#' data into R, we often do so with sf::read_sf(). This function converts the
#' latter to the former using geojsonio
#'
#' @param cell_geom an sf object which describes one cell
#'
#' @return a nested list with the coordinates of the polygon
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
