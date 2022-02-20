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

coleo_cell_geom_fmt <- function(cell_geom){
  # take a single sf class polygon and return the nested list suitable for injection

  # test if object is sf or not.

  # should work on the whole dataframe or just on one column?

  # My instinct is that it's clearer to use functions that work on one row at a
  # time, rather than on the whole data.frame at a time. If something like that can be worked out,
}

# cell injections are done with coleo_inject_general
