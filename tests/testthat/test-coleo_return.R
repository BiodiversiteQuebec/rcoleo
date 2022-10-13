test_that("coleo_return_* functions return object of the right type", {

  # coleo_return_valid_campaigns
  expect_vector(coleo_return_valid_campaigns())

  # coleo_return_valid_site_types
  expect_vector(coleo_return_valid_site_types())

  # coleo_return_required_cols
  expect_vector(coleo_return_required_tables("ADNe"))

  # Test coleo_return_required_name_table
  expect_vector(coleo_return_required_name_table("sites"))

  # Test coleo_return_name_table
  expect_vector(coleo_return_name_table("sites"))

  # test coleo_return_rename_vec_input_to_db
  table <- "campaigns"
  expect_vector(coleo_return_rename_vec_input_to_db(table))
  df_col_names <- coleo_make_df_column_names(table,
                                             coleo_get_column_names(table)$column_name)
  names(df_col_names) <- coleo_get_column_names(table)$column_name
  expect_mapequal(coleo_return_rename_vec_input_to_db(table), df_col_names)

})
