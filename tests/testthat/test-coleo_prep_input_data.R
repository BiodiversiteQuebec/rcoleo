test_that("coleo_prep_input_data works as expected", {

  fake_pap_data <- tibble::tribble(
    ~cell_code,    ~site_code,   ~type_hab,       ~lat,        ~lon,   ~opened_at,   ~closed_at,          ~type,       ~technician_1, ~temp_c, ~sky, ~wind,     ~category,       ~vernacular_fr,    ~rank,         ~taxa_name, ~abondance, ~tsn,
    "131_120", "131_120_H01", "tourbière", "48.57094", "-70.86202", "2018-07-05", "2018-07-05", "papillonidés", "Ariel Ferland-Roy",    "32",  "1",   "3", "arthropodes", "croissant nordique", "espèce", "Phyciodes cocyta",          1,   NA,
    "136_95",  "136_95_H01", "tourbière", "45.99011", "-73.30001", "2018-06-26", "2018-06-26", "papillonidés",   "Virginie Boivin",    "24",  "1",   "3", "arthropodes",                 "NA",  "genre",          "Crambus",          1,   NA,
    "136_116", "136_116_H01", "tourbière", "47.60274", "-70.97671", "2018-07-13", "2018-07-13", "papillonidés",      "Adine Seguin",    "22",  "2",   "4", "arthropodes",  "coliade intérieur", "espèce",  "Colias interior",          1,   NA
  )


  prep_input_data(fake_pap_data, "sites")


})
