
coleo_process_site_resp <- function(resp){
  # works but is ugly
  resp_list <- resp |>
    httr2::resp_body_json()


  # this relatively awkward -- but the challenge here is the incompatible types
  # of the different columns. for this reason we keep the empty values, then we pivot, THEN we drop them, then we unnest them
  # See the article on "rectangling" that does with dplyr
  full_data <- tibble::tibble(data = resp_list) |>
    tidyr::unchop(data, keep_empty = FALSE) |>
    dplyr::mutate(nm = names(data)) |>
    tidyr::pivot_wider(names_from = nm, values_from = data) |>
    ## DROP NULL COLUMNS
    purrr::discard(~ is.null(.x[[1]]))


  site_data <- full_data |>
    tidyr::unnest(!c("campaigns", "geom", "cell"))  |>
    tidyr::hoist(cell,  cell_name = "name")

  return(site_data)
}

coleo_process_site_df <- function(site_df){

  campaign_level_info <- site_df |>
    tidyr::hoist(cell, cell_name = "name") |>
    dplyr::select(-cell, -geom) |>
    tidyr::unnest_longer(campaigns) |>
    tidyr::hoist(campaigns,
                 campaign_type = "type",
                 campaign_id = "id") |>
    dplyr::rename(site_id = id)

  return(campaign_level_info)
}
