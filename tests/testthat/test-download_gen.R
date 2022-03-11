context("downloading and augmenting dataframe")

test_that("check download_all_requests works", {

  one_site <- download_sites_sf(site_code = "148_101_F01")

  # download_all_requests(one_site,
  #                       request_info_col = "campaigns",
  #                       query_info = c("type",
  #                                      "opened_at",
  #                                      "closed_at",
  #                                      "site_id"),
  #                       endpoint = "campaigns",
  #                       request_col_name = "camp_resp")
  #
  expect_s3_class(one_site, "data.frame")

})
