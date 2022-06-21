test_that("coleo_prep_input_data works as expected", {
  fake_pap_data <-
    tibble::tribble(
      ~sites_site_code, ~sites_type_hab, ~sites_lat, ~sites_lon, ~campaigns_opened_at, ~campaigns_closed_at, ~campaigns_type, ~campaigns_technicians, ~environments_temp_c, ~environments_sky, ~environments_wind, ~ref_species_category, ~ref_species_vernacular_fr, ~ref_species_taxa_rank, ~ref_species_taxa_name, ~obs_species_abondance, ~ref_taxa_tsn, ~observations_extra_variable_1, ~observations_extra_description_1, ~observations_extra_value_1, ~observations_extra_units_1,
      "131_120_H01", "tourbière", 48.57094, -70.86202, "2018-07-05", "2018-07-05", "papillonidés", "Ariel Ferland-Roy", "32", "1", "3", "arthropodes", "croissant nordique", "espèce", "Phyciodes cocyta", 1, NA, "longueur_poisson", "Longueur du poisson", 10, "cm",
      "136_95_H01", "tourbière", 45.99011, -73.30001, "2018-06-26", "2018-06-26", "papillonidés", "Virginie Boivin", "24", "1", "3", "arthropodes", "NA", "genre", "Crambus", 1, NA, "longueur_poisson", "Longueur du poisson", 12, "cm",
      "136_116_H01", "tourbière", 47.60274, -70.97671, "2018-07-13", "2018-07-13", "papillonidés", "Adine Seguin", "22", "2", "4", "arthropodes", "coliade intérieur", "espèce", "Colias interior", 1, NA, "longueur_poisson", "Longueur du poisson", 11.045, "cm"
    )


  nested_df <- coleo_prep_input_data(fake_pap_data, "campaigns")

  expect_equal(nrow(nested_df), 3)

  expect_equal(ncol(nested_df$data[[1]]), 17)

  expect_s3_class(nested_df, "rowwise_df")

  expect_equal(names(nested_df), c("site_id", "type", "technicians", "opened_at", "closed_at", "data"))

  # Test extra columns
  ## Tests that an extra column is returned when there is columns table_extra_xxx
  df_test <- data.frame(
    col_1 = 1:6,
    observations_extra_variable_1 = c(rep("a_variable", 5), NA),
    observations_extra_value_1 = c(1:3, NA, 5:6),
    observations_extra_units_1 = rep("m", 6)
  )
  out_tbl <- coleo_format_extra_col(df_test, "observations")
  out_tbl_1 <- coleo_format_extra_col(df_test[1, ], "observations")
  exp_json_1 <- jsonlite::toJSON(list(a_variable = list(value = 1, units = "m")), auto_unbox = TRUE)
  out_tbl_4 <- coleo_format_extra_col(df_test[4, ], "observations")
  exp_json_4 <- jsonlite::toJSON(list(a_variable = list(value = "", units = "m")), auto_unbox = TRUE)
  ### Tests
  testthat::expect_s3_class(out_tbl, "data.frame") # Format
  testthat::expect_equal(names(out_tbl), c("col_1", "observations_extra")) # Expected columns are returned
  testthat::expect_identical(out_tbl_1[[2]], exp_json_1) # Expected json format
  testthat::expect_identical(out_tbl_4[[2]], exp_json_4) # Expected handling of NAs

  ## Test for handling of multiple extra variables
  df_test_2_extra_cols <- data.frame(
    col_1 = 1:6,
    observations_extra_variable_1 = c(rep("taxonomist", 5), NA),
    observations_extra_value_1 = c(1:3, NA, 5:6),
    observations_extra_variable_2 = c(rep("helper", 5), NA),
    observations_extra_value_2 = c(1:3, NA, 5:6),
    observations_extra_units_2 = rep("mm", 6)
  )
  out_tbl_extra_4 <- coleo_format_extra_col(df_test_2_extra_cols[4, ], "observations")
  out_tbl_extra_1 <- coleo_format_extra_col(df_test_2_extra_cols[1, ], "observations")
  exp_json_two_variables <- jsonlite::toJSON(list(taxonomist = list(value = 1), helper = list(value = 1, units = "mm")), auto_unbox = TRUE)
  ### Tests
  testthat::expect_equal(names(out_tbl_extra_4), c("col_1", "observations_extra")) # Expected columns are returned
  testthat::expect_identical(out_tbl_extra_1[[2]], exp_json_two_variables) # Expected formating of NAs

  ## Test for multiple extra columns in different tables
  df_test_3_extra <- data.frame(
    col_1 = 1:6,
    observations_extra_variable_1 = c(rep("taxonomist", 5), NA),
    observations_extra_value_1 = c(1:3, NA, 5:6),
    environments_extra_variable_1 = c(rep("wind", 5), NA),
    environments_extra_value_1 = c(1:3, NA, 5:6)
  )
  out_tbl_extra_cols_1 <- coleo_format_extra_col(df_test_3_extra[1, ], "environments")
  testthat::expect_equal(names(out_tbl_extra_cols_1), c("col_1", "observations_extra_variable_1", "observations_extra_value_1", "environments_extra")) # Returns expected columns
})