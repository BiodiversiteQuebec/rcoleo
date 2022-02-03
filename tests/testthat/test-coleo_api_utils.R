test_that("api utils do the right thing", {

  setup_call <- coleo_begin_req()
  expect_s3_class(setup_call, class = "httr2_request")

  expect_equal(setup_call$url, "https://coleo.biodiversite-quebec.ca/api/v1")

})


