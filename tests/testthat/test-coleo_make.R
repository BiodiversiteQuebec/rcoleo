test_that("coleo_make_df_column_names returns a vector composed of properly formated colnames", {

  tbl <- "this_works"

  db_col_names = c("properly", "perfectly")

  col_names <- coleo_make_df_column_names(tbl, db_col_names)

  expect_vector(col_names)

  expect_equal(col_names, c("this_works_properly", "this_works_perfectly"))
})
