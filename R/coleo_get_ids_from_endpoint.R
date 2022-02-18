
get_ids_from_table <- function(df, db_table){

  if(db_table == "taxa") {
    query_fields <- "name"
  } else {
    query_fields <- coleo_get_name_table_column(db_table, "db_column")
  }

  if (db_table == "campaigns") {
    query_fields <- query_fields |> drop_techs()
  } else if (db_table == "observations") {
    query_fields <- c("date_obs", "campaign_id")
  }
  ## IF there are other restrictions on what should be "asked" of the db, put them here.

  in_coleo <- df |>
    mutate(id_db = purrr::pmap(select(.,
                                      any_of(query_fields)),
                               coleo_gen_req,
                               table = db_table))

  return(in_coleo)
}


# need a rowwise data frame if dealing with site_code
coleo_prep_input_site_code <- function(df){
  df |>
    dplyr::nest_by(site_code)
}
