test_that("coleo_return_* functions return object of the right type", {

  # coleo_return_valid_campaigns
  expect_vector(coleo_return_valid_campaigns())
  valid_campaigns <- coleo_return_valid_campaigns()
  expect_true(all(c("ADNe", "NDSI") %in% valid_campaigns))

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

  # Test coleo_return_campaign_type
  ## Returns the campaign type from a data frame
  c_type <- coleo_return_campaign_type(data.frame(campaigns_type = "ADNe", campaigns_date_start = "2020-07-22"))
  expect_equal(c_type, "ADNe")
  ## Returns the remote sensing indicator from a data frame
  rs_type <- coleo_return_campaign_type(data.frame(remote_sensing_indicators_name = "NDSI", remote_sensing_events_date_start = "2020-07-22"))
  expect_equal(rs_type, "NDSI")
})
