

without_internet({
  test_that("coleo_request makes a good request", {

    expect_GET(coleo_request_general(endpoint = "cells", response_as_df = FALSE, schema = "public", 'cell_code' = "eq.foo"),
               url = "https://coleo.biodiversite-quebec.ca/newapi/v2/cells?cell_code=eq.foo")
  })
})


test_that("no answer for a nonsense code", {
  nonsense_request <- coleo_request_general(endpoint = "cells", response_as_df = FALSE, schema = "public", 'cell_code' = "eq.foo")
  expect_equal(httr2::resp_body_json(nonsense_request), list())
})

# query for a real site
test_that("returns answer for a real code",{
  real_site <- coleo_request_general(endpoint = "cells", schema = "public", "cell_code" = "eq.139_87")
  answer <- httr2::resp_body_json(real_site)[[1]]
  expect_equal(length(answer), 6)
})


real_cell <- coleo_request_general(endpoint = "cells", schema = "public", "cell_code" = "eq.139_87")
real_cell_df <- coleo_request_general(endpoint = "cells", schema = "public", "cell_code" = "eq.139_87", response_as_df = TRUE)
resp_body <- httr2::resp_body_json(real_cell)[[1]]
test_that("response is in expected format", {
  expect_equal(length(resp_body), 6)
  # can pluck one id successfully from site request
  expect_equal(coleo_extract_id(real_cell), 161)
  expect_type(real_cell_df, "list")
})

test_that("errors for bad endpoint", {
  expect_error(coleo_request_general(endpoint = "not_an_endpoint"))
})

# Site id download and extraction
real_site <- coleo_request_general(endpoint = "sites", schema = "public", "site_code" = "eq.139_87_F01")
test_that("request_general works for a valid site", {
  resp_body <- httr2::resp_body_json(real_site)[[1]]
  # should be 11 columns of info for this site
  expect_equal(length(names(resp_body)), 12)
})
test_that("can coleo_pluck_id correctly", {
  expect_equal(coleo_extract_id(real_site), 94)
})

# Site processing works correctly
resp_df <- coleo_request_general(endpoint = "sites", response_as_df = TRUE, schema = "public", "site_code" = "eq.137_107_H02")
test_that("resp is easily processed", {
  # This processing step returns everything in one row
  expect_equal(nrow(resp_df), 1)
  # things should be unnested now; here is one example
  expect_equal(length(resp_df$id), 1)
  # coluns should be of the right class
  expect_type(resp_df$type, "character")
})


test_that("coleo_request_data returns expected output", {
  # Test case 1: survey_type = 'vegetation', view = 'short'
  result1 <- coleo_request_data(survey_type = 'vegetation', view = 'short')
  expect_is(result1, "tbl_df")
  expect_equal(ncol(result1), 20) # Assuming there are 18 columns of data for vegetation inventory

  # Test case 2: survey_type = 'acoustique_anoures', view = 'long'
  result2 <- coleo_request_data(survey_type = 'acoustique_anoures', view = 'long')
  expect_is(result2, "tbl_df")
  expect_equal(ncol(result2), 22) # Assuming there are 22 rows of data for acoustique_anoures inventory

  # Add more test cases for other survey types and views...
})

# with_mock_dir("coleo_request_by_code", {
#   # request ALL the cells
#   all_cells <- coleo_request_by_code(human_code = NULL, table = "cells")
#   all_cells_list <- all_cells |> httr2::resp_body_json()
#   expect_(50L, all_cells_list |> length())
# })



# is any of this needed?

if (FALSE) {












  resp |>
    httr2::resp_body_json(.) |>
    tibble::tibble(data = .) |>
    dplyr::mutate(data = list(head(data[[1]], 5))) |>
    tidyr::unnest_wider(data,
                        ptype = c(id = integer(0),
                                  cell_id = integer(0),
                                  off_station_code_id = vctrs::unspecified(),
                                  site_code = character(0),
                                  type = character(0),
                                  opened_at = character(0),
                                  geom = "named list",
                                  created_at = character(0),
                                  updated_at = character(0),
                                  cellId = integer(0),
                                  campaigns = list(0),
                                  cell = list(0)))



  tidyr::hoist(cell, cell_namecell_name  = "name")

  if (any("cell" == names(resp_no_null))) {
    resp_no_null$cell_namecell_name  <- resp_no_null$cell$name
    resp_no_null[["cell"]] <- NULL
    # make it into a tibble and put it in a list of length 1
    # resp_no_null$cell<- list(tibble::as_tibble(resp_no_null$cell))
  }

  df_for_all <- resp_no_null |>
    purrr::map_if(~length(.)>1, ~ list(tibble::tibble(data = .))) |>
    tibble::as_tibble()

  df_for_all$campaigns[[1]] |> tidyr::unnest_wider(data)


  site_campaign_data <- df_for_all |>
    tidyr::unnest(campaigns) |>
    tidyr::hoist(data,
                 campaign_id = "id",
                 campaign_type = "type") |>
    dplyr::rename(campaign_data = data)

  site_campaign_data


  old_style <- get_sites(site_code='137_107_H02')
  old_style[[1]]$body[[1]]$campaigns[[1]]
}
