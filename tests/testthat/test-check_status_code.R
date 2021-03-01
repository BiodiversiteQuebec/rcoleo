context("test the status code")


# NOTE requires rcoleo token!

test_that("status code informs of an error", {

  bad_post <- rcoleo::post_gen(rcoleo:::endpoints()$cells, "foo")
  # don't work if it's in a list
  expect_error(check_status_code(list(bad_post)))

  expect_equal(check_status_code(bad_post), 400L)

  # check the list

  expect_message(check_post_list(list(bad_post, bad_post)),
                "Oups! une probl\u00e8me est survenu: 400, 400")


  expect_message(post_campaigns("foo"), "Oups! une probl\u00e8me est survenu: 400")


})


