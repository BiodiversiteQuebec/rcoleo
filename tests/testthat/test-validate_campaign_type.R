test_that("validate campaigns", {
  expect_equal(validate_campaign_type("v"), "végétation")

  expect_error(validate_campaign_type("vege"))
  expect_error(validate_campaign_type("decomp"))

  })
