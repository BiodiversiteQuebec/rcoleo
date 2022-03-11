test_that("coleo_name_table_functions", {
  expect_equal(coleo_return_valid_campaigns(),
               c("végétation", "végétation_transect", "sol", "acoustique",
                 "phénologie", "mammifères", "papilionidés", "odonates", "insectes_sol",
                 "ADNe", "zooplancton", "température_eau", "température_sol",
                 "marais_profondeur_température"
                 ))

  expect_equal(coleo_return_valid_site_types(),
               c("lac", "rivière", "forestier", "marais", "marais côtier",
                 "toundrique", "tourbière"
               ))

  expect_error(coleo_get_name_table_column("foo"))

  expect_equal(coleo_get_name_table_column("sites"),
               c("site_code", "site_name",
                 "site_type", "site_date_opened",
                 "site_lat", "site_lon", "site_notes", "cell_id"
               ))

  expect_equal(coleo_get_name_table_column("sites", column = "db_column"),
               c("site_code", "site_name",
                 "type", "opened_at", "lat", "lon", "notes", "cell_id"))

  expect_error(coleo_get_name_table_column("sites", column = "foobar"), regexp = "column should.*")


  expect_equal(coleo_get_rename_vec_input_to_db("campaigns"),
               c(type = "campaign_type", technician_1 = "campaign_technician_1", technician_2 = "campaign_technician_2",
                 opened_at = "campaign_date_opened", closed_at = "campaign_date_closed",
                 notes = "campaign_notes"
                 ))

  expect_equal(coleo_get_required_name_table("sites"),
               c("site_code", "site_name", "site_type", "site_date_opened",
                 "site_lat", "site_lon"
                 ))

})
