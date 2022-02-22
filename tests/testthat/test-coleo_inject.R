with_mock_dir("inject a test site", {

  one_cell_list <- list(type = "Polygon",
                        coordinates = list(
                          list(c(-79.340288, 48.511171),
                               c(-79.451258, 48.4084),
                               c(-79.5175150698258, 48.43971636114),
                               c(-79.5176355352339, 48.5000133539952),
                               c(-79.517581626098, 48.503401649012),
                               c(-79.5175785798052, 48.5615006339734),
                               c(-79.5175785437023, 48.5643904071127))))

  demo_test <- coleo_inject_general(cell_code = "FFF_XXX",
                                    name ="Middle Earth",
                                    geom = one_cell_list,
                                    endpoint = "cells")
  # demo_test$body

  demo_result <- httr2::req_perform(demo_test)

  test_that("request body is a named list", {
    expect_equal(names(demo_test$body$data), c("cell_code", "name", "geom"))
    expect_equal(names(demo_test$body$data$geom), c("type", "coordinates"))
    expect_type(demo_test$body$data, "list")
  })

  test_that("ID for new record is a number", expect_gt(coleo_pluck_one_id(demo_result), 1L))

})
