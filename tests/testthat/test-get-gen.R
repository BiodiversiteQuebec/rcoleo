context("gen_gen")

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

  # download_sites_sf(site_code = c("148_101_F01", "145_141_H01"))



  ## need to restructure the code and test all the downloading function
  # one_site <- download_sites_sf(site_code = "148_101_F01")
  #
  # query_df_gen(one_site, query_info = "site_code", endpoint = "species_list")
  # # this last line is wrong
  # query_df_gen(one_site$campaigns[[1]], query_info = "campaign_id", endpoint = "observations")
})
