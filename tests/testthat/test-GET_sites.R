context("get_sites")

test_that("coleoGetResp to data.frame works", {
  one_answer <- get_sites(site_code=c('135_104_H01'))

  one_answer_df <- as.data.frame(one_answer)

  expect_s3_class(one_answer, "coleoGetResp")

  expect_s3_class(one_answer_df, "data.frame")

  expect_equal(nrow(one_answer_df), 1)



})

test_that("works for a vector", {

  two_answer <-  get_sites(site_code = c("148_101_F01", "145_141_H01"))

  expect_equal(length(two_answer), 2)

  expect_null(names(two_answer))

  expect_equal(names(two_answer[[1]]), names(two_answer[[2]]), c("body", "response"))

  })
