test_that("richness downloads correctly", {

  full_table <- get_richness()

  expect_s3_class(full_table, "tbl_df")

  expect_equal(nrow(full_table), 1)


  get_richness(campaign_type = "ac")

  get_richness(campaign_type = "in")


  get_richness(campaign_type = "v")

  })
