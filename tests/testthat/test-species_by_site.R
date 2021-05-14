
two_sites <- c("124_86_H01", "145_141_H01")
resp_2 <- get_species_list(site_code = two_sites)



test_that("species_by_site returns list", {

  # default
  resp <- get_species_list(site_code = "145_141_H01")

  expect_type(resp, "list")

  expect_type(resp_2, "list")

  expect_named(resp_2, two_sites)


  # for vegetation
  resp_veg <- get_species_list(campaign_type = "v", site_code = "148_101_H01")

  expect_type(resp_veg, "list")


  expect_s3_class(resp_veg[[1]], "tbl_df")

  expect_gt(nrow(resp_veg[[1]]), 0)

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

