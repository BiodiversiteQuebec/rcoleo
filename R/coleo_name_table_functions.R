# functions that work with get_name_table()


get_name_table_column <- function(db_table, column = "input_column"){

  nm_tbl <- coleo_get_name_table()

  assertthat::assert_that(db_table %in% unique(nm_tbl$table),
                          msg = "Not one of the database tables")

  assertthat::assert_that(column %in% names(nm_tbl),
                          msg = paste0("column should be one of: ",
                                       paste(names(nm_tbl), collapse = ", ")
                                       ))

  input_names <- split(nm_tbl[[column]], nm_tbl$table)

  out <- input_names[[db_table]]

  if (db_table == "observations") {
    out <- c(out, "campaign_id")
  }

  return(out)

}

get_rename_vec_input_to_db <- function(db_table){
  inputs <- get_name_table_column(db_table)
  upload <- get_name_table_column(db_table, column = "db_column")
  purrr::set_names(inputs, upload)
}
