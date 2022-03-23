test_that("coleo_return_* functions return object of the right type", {

  # coleo_return_valid_campaigns
  expect_vector(coleo_return_valid_campaigns())

  # coleo_return_valid_site_types
  expect_vector(coleo_return_valid_site_types())

  # coleo_return_required_cols
  expect_vector(coleo_return_required_tables("acoustique"))
})
