test_that("coleo_get_name_table makes a table", {

  nm_tbl <- coleo_get_name_table()

  testthat::expect_s3_class(nm_tbl, "tbl_df")

  expect_equal(dput(sapply(nm_tbl, class)),
               c(table = "character", input_column = "character", db_column = "character",
                 required_class = "list", legal_values = "list"))

  # confirm function
  expect_type(nm_tbl$required_class[[1]], "builtin")



})
