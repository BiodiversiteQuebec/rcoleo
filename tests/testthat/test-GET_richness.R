test_that("richness downloads correctly", {

  full_table <- get_richness()

  expect_s3_class(full_table, "tbl_df")

  expect_equal(nrow(full_table), 1)

  })
