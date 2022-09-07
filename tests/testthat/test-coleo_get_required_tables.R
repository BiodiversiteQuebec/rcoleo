test_that("coleo_get_required_tables makes a table", {

  local_edition(3)
  nm_tbl <- coleo_get_required_tables()

  testthat::expect_s3_class(nm_tbl, "tbl_df")
})
