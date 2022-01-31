test_that("format spatial gives errors for bad inputs, correct format for good oens", {

  test <- data.frame(foo = "bar", lat = -70, longitude = 47)

  expect_error(format_spatial(test))

  test2 <- data.frame(foo = "bar", latitude = -70, lon = 47)

  expect_error(format_spatial(test2))

  test_latlon_only <- tibble::tibble(foo = "bar", lat = -70, lon = 47)

  formatted_output <- format_spatial(test_latlon_only)

  expect_type(object = formatted_output$geom, type = "list")

  expect_equal(names(formatted_output$geom[[1]]), c("type", "coordinates"))

  })
