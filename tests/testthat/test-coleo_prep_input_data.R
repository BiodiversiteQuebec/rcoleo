test_that("coleo_prep_input_data works as expected", {

  fake_pap_data <-
tibble::tribble(
          ~sites_site_code,   ~type_hab, ~sites_lat, ~sites_lon,   ~sites_opened_at,   ~sites_closed_at,          ~campaigns_type,       ~campaigns_technician, ~temp_c, ~sky, ~wind,     ~category,       ~vernacular_fr,    ~ref_taxa_rank,         ~ref_taxa_taxa_name, ~ref_taxa_abondance, ~ref_taxa_tsn, ~observations_extra_variable, ~observations_extra_description, ~observations_extra_value, ~observations_extra_units,
       "131_120_H01", "tourbière",  48.57094, -70.86202, "2018-07-05", "2018-07-05", "papillonidés", "Ariel Ferland-Roy",    "32",  "1",   "3", "arthropodes", "croissant nordique", "espèce", "Phyciodes cocyta",          1,   NA, "longueur_poisson", 'Longueur du poisson', 10, "cm",
        "136_95_H01", "tourbière",  45.99011, -73.30001, "2018-06-26", "2018-06-26", "papillonidés",   "Virginie Boivin",    "24",  "1",   "3", "arthropodes",                 "NA",  "genre",          "Crambus",          1,   NA, "longueur_poisson", 'Longueur du poisson', 12, "cm",
       "136_116_H01", "tourbière",  47.60274, -70.97671, "2018-07-13", "2018-07-13", "papillonidés",      "Adine Seguin",    "22",  "2",   "4", "arthropodes",  "coliade intérieur", "espèce",  "Colias interior",          1,   NA, "longueur_poisson", 'Longueur du poisson', 11.045, "cm"
       )


  nested_df <- coleo_prep_input_data(fake_pap_data, "sites")

  expect_equal(nrow(nested_df), 3)

  expect_equal(ncol(nested_df$data[[1]]), 17)

  expect_s3_class(nested_df, "rowwise_df")

  # campaign test should work too -- pointless with this data, since it has no site_id
  nested_camps <- coleo_prep_input_data(fake_pap_data, "campaigns")

  expect_s3_class(nested_camps, "rowwise_df")

  # Test extra columns
  ### Tests that an extra column is returned when there is columns table_extra_xxx
  df_test <- data.frame(
    col_1 = 1:6,
    observations_extra_variable = c(rep("a_variable", 5), NA),
    observations_extra_value = c(1:3,NA,5:6),
    observations_extra_units = rep("m", 6)
  )
  out_tbl <- coleo_format_extra_col(df_test, "observations")
  out_tbl_1 <- coleo_format_extra_col(df_test[1,], "observations")
  exp_json_1 <- jsonlite::toJSON(list(a_variable = list(value = 1, units = 'm')), auto_unbox = TRUE)
  out_tbl_4 <- coleo_format_extra_col(df_test[4,], "observations")
  exp_json_4 <- jsonlite::toJSON(list(a_variable = list(units = 'm')), auto_unbox = TRUE)

  testthat::expect_s3_class(out_tbl, "data.frame")
  testthat::expect_equal(names(out_tbl), c("col_1","extra"))
  testthat::expect_identical(out_tbl_1[[2]], exp_json_1)
  testthat::expect_identical(out_tbl_4[[2]], exp_json_4)

})
