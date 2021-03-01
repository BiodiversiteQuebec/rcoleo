test_that("coleoGetResp to data.frame works", {
  one_answer <- get_sites(site_code=c('135_104_H01'))

  one_answer_df <- as.data.frame(one_answer)

  expect_s3_class(one_answer, "coleoGetResp")

  expect_s3_class(one_answer_df, "data.frame")

  expect_equal(nrow(one_answer_df), 1)



})
