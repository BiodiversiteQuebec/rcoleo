

test_that("species_by_site returns list", {

  # default
  resp <- get_species_list(site_code = "145_141_H01")

  expect_type(resp, "list")


  # for vegetation
  resp_veg <- get_species_list(campaign_type = "v", site_code = "148_101_H01")

  expect_type(resp_veg, "list")


  expect_gt(nrow(resp_veg), 0)

})

test_that("get_species_list works for one NULL or another, but not both", {
  resp_veg <- get_species_list("v")
  expect_gt(nrow(resp_veg), 100)


  resp_onesite <- get_species_list(site_code = "148_101_H01")

  # greather than 10, arbitrary number
  expect_gt(nrow(resp_onesite), 10)

  expect_error(get_species_list(campaign_type = NULL, site_code = NULL))

})


test_that("can get a binary sxs matrix", {

  toy_list <- list(
    first_site = data.frame(taxa_name = letters[1:6]),
    second_site = data.frame(taxa_name = letters[3:7])
  )


  answer <- matrix(c(1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1),
                   nrow = 2L,
                   ncol = 7L,
                   dimnames = list(c("first_site", "second_site"), letters[1:7]))

  expect_equal(species_site_matrix(toy_list), answer)

})

