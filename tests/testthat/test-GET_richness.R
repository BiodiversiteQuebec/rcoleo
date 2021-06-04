test_that("richness downloads correctly", {

  full_table <- get_richness()

  expect_s3_class(full_table, "tbl_df")

  expect_gt(nrow(full_table), 3)

  })
