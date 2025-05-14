

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
  answer <- coleo_request_general(endpoint = "cells", schema = "public", "cell_code" = "eq.139_87")
  expect_equal(length(answer[[1]]), 6)
})


real_cell <- coleo_request_general(endpoint = "cells", schema = "public", "cell_code" = "eq.139_87")
real_cell_df <- coleo_request_general(endpoint = "cells", schema = "public", "cell_code" = "eq.139_87", response_as_df = TRUE)
resp_body <- real_cell[[1]]
test_that("response is in expected format", {
  expect_equal(length(resp_body), 6)
  # can pluck one id successfully from site request
  expect_equal(resp_body$id, 161)
  expect_type(real_cell_df, "list")
})

test_that("errors for bad endpoint", {
  expect_error(coleo_request_general(endpoint = "not_an_endpoint"))
})

# Site id download and extraction
real_site <- coleo_request_general(endpoint = "sites", schema = "public", "site_code" = "eq.139_87_F01")
test_that("request_general works for a valid site", {
  resp_body <- real_site[[1]]
  # should be 11 columns of info for this site
  expect_equal(length(names(resp_body)), 12)
})
test_that("can extract id correctly", {
  expect_equal(resp_body$id, 94)
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

test_that("coleo_request_data handles errors correctly", {
  # Test case with an invalid survey_type
  expect_error(coleo_request_data(survey_type = 'invalid_survey_type'))
})

test_that("coleo_request_general handles absence of data correctly", {
  # Test case with a non-existent endpoint
  expect_error(coleo_request_general(endpoint = "non_existent_endpoint"))
  # Test with existing endpoint but no data
  expect_length(coleo_request_general(endpoint = "cells", schema = "public", 'cell_code' = "eq.non_existent_code"), 1)
  expect_equal(coleo_request_general(endpoint = "cells", schema = "public", 'cell_code' = "eq.non_existent_code")[[1]], list())
})

test_that("coleo_request_general handles large parameters correctly", {
  # Test with a large number of parameters
  expect_error(coleo_request_general(endpoint = "cells", perform = TRUE, response_as_df = FALSE, schema = 'api', 'param' = paste0("in.(",paste(rep("value", 10000), collapse = ","), ")")), "Trop d'éléments")
})