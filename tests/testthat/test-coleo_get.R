test_that("coleo_get_* functions return an object of the right class", {

  # test coleo_get_rename_vec_input_to_db
  table <- "campaigns"
  expect_vector(coleo_get_rename_vec_input_to_db(table))
  df_col_names <- coleo_make_df_column_names(table,
                                             coleo_get_column_names(table)$column_name)
  names(df_col_names) <- coleo_get_column_names(table)$column_name
  expect_mapequal(coleo_get_rename_vec_input_to_db(table), df_col_names)

  # Test coleo_get_name_table
  expect_vector(coleo_get_name_table("sites"))

  # Test coleo_get_required_name_table
  expect_vector(coleo_get_required_name_table("sites"))

  # Test coleo_get_column_names
  tbl <- coleo_get_column_names("sites")
  expect_s3_class(tbl, "data.frame")
  expect_equal(sapply(tbl, class),
               c(column_name = "character", data_type = "character", udt_name = "character",
                 is_nullable = "character"
               ))

  # Test coleo_get_enum_values
  expect_vector(coleo_get_enum_values("enum_sites_type"))

  # Test coleo_get_attributes_table
  attributes_tbl <- coleo_get_attributes_table()
  expect_s3_class(attributes_tbl, "data.frame")
  expect_equal(sapply(attributes_tbl, class),
               c(variable = "character", description = "character", unit = "character",
                 created_at = "character", updated_at = "character"))
  attributes_col <- coleo_get_attributes_table(column = "variable")
  expect_vector(attributes_col)
})
