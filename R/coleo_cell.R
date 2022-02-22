# data about cells needs to be validated and prepared in a certain way before
# being injected. These functions make those checks. coleo_cell_df will also
# return a data.frame for a specific cell


coleo_cell_response_format <- function(coleo_resp) {
  #take a coleo response for a cell and return a nice data.frame
}

coleo_cell_df <- function(cell_code) {
  #with a cell code, return a df with all cell info: name, geom
}

coleo_cell_validate <- function(df) {
  # take a df containing some cells to validate. check the data
  # cell_code correct # characters, one underscore
  # name -- no NA
  # geom is a nested list, of the kind make by coleo_cell_geom_fmt
}

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
  if (!any("sfg" == class(cell_geom))) stop("This function is meant to work on sf objects.")

  one_cell_list <- geojsonio::geojson_list(cell_geom)

  one_cell_list$coordinates <- list(
    setNames(split(one_cell_list$coordinates[[1]],
                   seq_len(nrow(one_cell_list$coordinates[[1]]))),
             NULL))

  attr(one_cell_list, which = c("class")) <- NULL
  attr(one_cell_list, which = c("from")) <- NULL

  return(one_cell_list)
}

# cell injections are done with coleo_inject_general


  # should work on the whole dataframe or just on one column?

  # My instinct is that it's clearer to use functions that work on one row at a
  # time, rather than on the whole data.frame at a time. If something like that can be worked out,

