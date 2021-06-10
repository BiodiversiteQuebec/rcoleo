test_that("can get a campaign table", {

  one_site_summary <- get_campaign_summary(site_code = "148_101_H01")

  expect_gt(nrow(one_site_summary), 1)

  # get_campaign_summary("v")

})
