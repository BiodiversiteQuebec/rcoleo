context("download_sites_sf works")

test_that("download_sites_sf works for vectors", {

  test_sites <- c("148_101_F01", "145_141_H01")
  ## download for checking
  n_sites <- download_sites_sf(site_code = test_sites)

  expect_equal(length(test_sites), nrow(n_sites))
})
