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
               c("cell_id", "off_station_code_id", "site_code", "site_name",
                 "site_type", "site_opened_at", "site_lat", "site_lon", "site_notes"
               ))

  expect_equal(coleo_get_name_table_column("sites", column = "db_column"),
               c("cell_id", "off_station_code_id", "site_code", "site_name",
                 "type", "opened_at", "lat", "lon", "notes"))

  expect_error(coleo_get_name_table_column("sites", column = "foobar"), regexp = "column should.*")


  expect_equal(coleo_get_rename_vec_input_to_db("campaigns"),
               c(site_id = "site_id", type = "camp_type", technician_1 = "technician_1",
                 technician_2 = "technician_2", opened_at = "camp_opened_at", closed_at = "camp_closed_at",
                 notes = "camp_notes"
                 ))

  expect_equal(coleo_get_required_name_table("sites"),
               c("cell_id", "site_code", "site_name", "site_type", "site_opened_at",
                 "site_lat", "site_lon"
                 ))

  })
