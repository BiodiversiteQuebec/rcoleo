with_mock_dir("coleo_get_ids from endpoint", {
  test_that("site_id from database", {

    testthat::local_edition(3)

    demo_data <- data.frame(site_code = c("139_87_F01", "139_87_F01"),
                            camp_type = c("acoustique", "acoustique"))

    prepared_demo <- coleo_prep_input_site_code(demo_data)

    expect_equal(nrow(prepared_demo), 1)

    expect_type(prepared_demo$data, "list")

    prepared_demo

    id_acquired <- prepared_demo |>
      dplyr::mutate(site_id = list(coleo_request_by_code(human_code = site_code, table = "sites")))

    expect_s3_class(id_acquired$site_id[[1]], "httr2_response")

    expect_type(coleo_extract_id(id_acquired$site_id[[1]]), "integer")

  })
})
