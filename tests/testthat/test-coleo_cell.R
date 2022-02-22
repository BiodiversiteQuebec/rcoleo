test_that("coleo cell formatting works", {

  demo_geom <- structure(list(structure(c(-79.340288, -79.451258, -79.5175150698258,
                             -79.5176355352339, -79.517581626098,
                             -79.5175785798052, -79.5175785437023,
                             -79.495668, -79.340288, 48.511171,
                             48.4084, 48.43971636114, 48.5000133539952,
                             48.503401649012, 48.5615006339734,
                             48.5643904071127, 48.584772,
                             48.511171), .Dim = c(9L, 2L))),
            class = c("XY", "POLYGON", "sfg"
                             ))

  expect_error(coleo_cell_geom_fmt(list(structure(c(-79.340288, -79.451258, -79.5175150698258,
                             -79.5176355352339, -79.517581626098,
                             -79.5175785798052, -79.5175785437023,
                             -79.495668, -79.340288, 48.511171,
                             48.4084, 48.43971636114, 48.5000133539952,
                             48.503401649012, 48.5615006339734,
                             48.5643904071127, 48.584772,
                             48.511171), .Dim = c(9L, 2L)))))


  formatted_polygon <- coleo_cell_geom_fmt(demo_geom)

  expect_equal(names(formatted_polygon), c("type", "coordinates"))
  expect_is(formatted_polygon$coordinates, "list")
  expect_equal(length(formatted_polygon$coordinates), 1L)

})

test_that("cell validation process works, returns good errors", {

  df <- tibble::tribble(
    ~cell_code, ~name,   ~geom,
    "XXX_FF", "Middle Earth",  structure(list(structure(c(-79.340288, -79.451258, -79.5175150698258,
                                                          -79.5176355352339, -79.517581626098,
                                                          -79.5175785798052, -79.5175785437023,
                                                          -79.495668, -79.340288, 48.511171,
                                                          48.4084, 48.43971636114, 48.5000133539952,
                                                          48.503401649012, 48.5615006339734,
                                                          48.5643904071127, 48.584772,
                                                          48.511171), .Dim = c(9L, 2L))),
                                         class = c("XY", "POLYGON", "sfg"
                                         ))
  )

  # df # should error
  # df$geom <- lapply(df$geom, coleo_cell_geom_fmt) # should be fine

  coleo_cell_validate()

})
