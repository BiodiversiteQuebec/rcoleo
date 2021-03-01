

test_that("post_gen_list works appropriately for inputs", {


  expect_error(post_gen_list(list(), endpoint = "foobar"))

  one_bad_post <- post_gen_list(list("foo"), "cells")

  expect_message(post_gen_list(list("foo"), "cells"), "Oups")

  expect_equal(length(one_bad_post), 1)

  expect_equal(class(one_bad_post), "coleoPostResp")


  })


test_that("POST_cells passes the correct class through", {
  bad_post <- post_cells(list("foo"))

  expect_equal(bad_post[[1]]$response$url,
  httr::modify_url(server(), path = paste0(base(), "/cells")))


  })
