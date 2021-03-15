test_that("query_df returns a df", {

  summ_resp <- query_gen("summary")

  expect_s3_class(summ_resp, "data.frame")


  expect_s3_class(summ_resp, "tbl_df")

  site_resp <- query_gen("sites")

  expect_s3_class(site_resp, "data.frame")


  camp_resp <- query_gen("campaigns")


  expect_s3_class(camp_resp,
                  "data.frame")

  })
