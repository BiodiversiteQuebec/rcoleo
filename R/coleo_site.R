
coleo_process_site_resp <- function(resp){
  # works but is ugly
  resp_list <- resp |>
    httr2::resp_body_json()

  full_data <- tibble::tibble(data = resp_list) |>
    tidyr::unchop(data, keep_empty = TRUE) |>
    dplyr::mutate(nm = names(data)) |>
    tidyr::pivot_wider(names_from = nm, values_from = data)

  site_data <- full_data |>
    tidyr::unnest(!c("campaigns", "geom", "cell"),
                  ptype =c(id = integer(0),
                           cell_id = integer(0),
                           off_station_code_id = vctrs::unspecified(),
                           site_code = character(0),
                           site_name = character(0),
                           type = character(0),
                           opened_at = character(0),
                           geom = "named list",
                           created_at = character(0),
                           updated_at = character(0),
                           cellId = integer(0),
                           campaigns = list(0),
                           cell = list(0)))  |>
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
