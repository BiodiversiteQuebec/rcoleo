test_that("coleo_prep_input_data works as expected", {

  fake_pap_data <-
tibble::tribble(
          ~sites_site_code,   ~type_hab, ~sites_lat, ~sites_lon,   ~sites_opened_at,   ~sites_closed_at,          ~campaigns_type,       ~campaigns_technician_1, ~temp_c, ~sky, ~wind,     ~category,       ~vernacular_fr,    ~ref_taxa_rank,         ~ref_taxa_taxa_name, ~ref_taxa_abondance, ~ref_taxa_tsn,
       "131_120_H01", "tourbière",  48.57094, -70.86202, "2018-07-05", "2018-07-05", "papillonidés", "Ariel Ferland-Roy",    "32",  "1",   "3", "arthropodes", "croissant nordique", "espèce", "Phyciodes cocyta",          1,   NA,
        "136_95_H01", "tourbière",  45.99011, -73.30001, "2018-06-26", "2018-06-26", "papillonidés",   "Virginie Boivin",    "24",  "1",   "3", "arthropodes",                 "NA",  "genre",          "Crambus",          1,   NA,
       "136_116_H01", "tourbière",  47.60274, -70.97671, "2018-07-13", "2018-07-13", "papillonidés",      "Adine Seguin",    "22",  "2",   "4", "arthropodes",  "coliade intérieur", "espèce",  "Colias interior",          1,   NA
       )


  nested_df <- coleo_prep_input_data(fake_pap_data, "sites")

  expect_equal(nrow(nested_df), 3)

  expect_equal(ncol(nested_df$data[[1]]), 15)

  expect_s3_class(nested_df, "rowwise_df")

  # campaign test should work too -- pointless with this data, since it has no site_id
  nested_camps <- coleo_prep_input_data(fake_pap_data, "campaigns")

  expect_s3_class(nested_camps, "rowwise_df")
})
