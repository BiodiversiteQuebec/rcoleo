test_that("api utils do the right thing", {

  setup_call <- coleo_begin_req()
  expect_s3_class(setup_call, class = "httr2_request")

  expect_equal(setup_call$url, "https://coleo.biodiversite-quebec.ca/api/v1")

})



# with_mock_dir("error message failed injection", {
#
#
#   # site_type mispelled -- a capital letter, missing cell_id
#   df <- tibble::tribble(
#     ~cell_code,    ~site_code,    ~site_name,  ~site_type, ~site_lat,  ~site_lon, ~site_date_opened,
#     "141_124", "141_124_F01", "Les Basques", "Forestier", 48.133559, -68.899209,      "2020-01-01"
#   )
#
#
#   inject_this_site_df <- coleo_prep_input_data(df, db_table = "sites")
#
#   site_data_req <- inject_this_site_df |>
#     dplyr::mutate(inject_request = list(
#       coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "sites")
#     ))
#
#   # this error is OK
#   expect_error(httr2::req_perform(site_data_req$inject_request[[1]]), regexp = ".*notNull Violation.*")
#
#   ## fix cell_id, site_type  still wrong
#   mispelled_req <- tibble::tribble(
#     ~cell_code,    ~site_code,    ~site_name,  ~site_type, ~site_lat,  ~site_lon, ~site_date_opened, ~cell_id,
#     "141_124", "141_124_F01", "Les Basques", "Forestier", 48.133559, -68.899209,      "2020-01-01",  4
#   ) |>
#     coleo_prep_input_data(db_table = "sites") |>
#     dplyr::mutate(inject_request = list(
#       coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "sites")
#     ))
#
#   expect_error(httr2::req_perform(mispelled_req$inject_request[[1]]), regexp = ".*enum.*")
#
#
# })
