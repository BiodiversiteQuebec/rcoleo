test_that("site_id from database", {

  testthat::local_edition(3)

  demo_data <- data.frame(site_code = c("139_87_F01", "139_87_F01"),
                          camp_type = c("acoustique", "acoustique"))

  prepared_demo <- coleo_prep_input_site_code(demo_data)

  expect_equal(nrow(prepared_demo), 1)

  expect_type(prepared_demo$data, "list")

  prepared_demo |>  dplyr::mutate(site_id = coleo_get_site_id(site_code))

})
