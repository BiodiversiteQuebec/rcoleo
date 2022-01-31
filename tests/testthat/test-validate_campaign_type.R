test_that("validate campaigns", {
  expect_error(validate_campaign_type("v"))

  #
  expect_equal(validate_campaign_type("végétation"), "végétation")
  expect_error(validate_campaign_type("decomp"))

  })
