context("test get_gen and query_gen")

test_that("API returns sensible error message when not authorised", {
  ## Once the api key is set up, you'll need to make it *not* available here, something like
  withr::with_envvar(c(RCOLEO_TOKEN = NA_character_), {
    expect_error(
      get_gen("/cells"),
      "Aucune autorisation d\u00e9tect\u00e9e")
  })


  spp_list <- get_gen(endpoint = endpoints()$species_list, query = list(site_code = "148_101_F01"))

  expect_s3_class(spp_list, "data.frame")

  expect_type(spp_list[[1]], "character")


  list_from_query <- query_gen("species_list", list(site_code = "148_101_F01"))

  expect_s3_class(list_from_query, "data.frame")

  expect_type(list_from_query[[1]], "character")

  })


test_that("can get species names from a site df", {

  test_sites <- c("148_101_F01", "145_141_H01")
  ## need to restructure the code and test all the downloading function
  n_sites <- download_sites_sf(site_code = test_sites)
  #
  resplist <- purrr::map(n_sites$site_code,
             ~ query_gen(endpoint = "species_list", list(site_code = .)))

  # should get back the same number of things
  expect_equal(length(test_sites), length(resplist))

})

test_that("can get all species names for a campaign",{

  resp <- query_gen("species_list", list(campaign_type = "acoustique"))

  expect_s3_class(resp, "data.frame")
  expect_type(resp[[1]], "character")
  expect_equal(ncol(resp), 3)

  })


test_that("both campaign type and site work",{

  resp <- query_gen("species_list", list(campaign_type = "acoustique",
                                         site_code = "145_141_H01"))

  expect_s3_class(resp, "data.frame")
  expect_type(resp[[1]], "character")
  expect_equal(ncol(resp), 3)

})


test_that("sites_species works",{

  # https://coleo.biodiversite-quebec.ca/api/v1/sites_species?campaign_type=insectes_sol
  raw_resp <- get_gen("/sites_species", list(campaign_type = "insectes_sol"))
  expect_equal(ncol(raw_resp), 2)
  expect_equal(sapply(raw_resp, typeof), c(site_code = "character", taxa_name = "character"))

  raw_resp2 <-  query_gen("sites_species", list(campaign_type = "insectes_sol"))

  expect_equal(raw_resp, raw_resp2)
  })
