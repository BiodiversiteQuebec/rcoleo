test_that("coleo_return_cols and coleo_return_required_cols make a table", {

  # coleo_return_cols
  expect_error(coleo_return_cols("foo"))

  nm_tbl <- coleo_return_cols("ADNe")

  expect_s3_class(nm_tbl, "data.frame")

  expect_equal(sapply(nm_tbl, class),
               c(noms_de_colonnes = "character", colonne_requise = "character", classe = "character",
                 valeurs_acceptees = "list"
               ))

  # required cols
  req_nm_tbl <- coleo_return_cols("ADNe", required.columns = TRUE)

  testthat::expect_equal(unique(req_nm_tbl$colonne_requise), "TRUE")
  testthat::expect_equal(req_nm_tbl$noms_de_colonnes, c("campaigns_type", "campaigns_opened_at", "landmarks_type", "observations_date_obs", "observations_is_valid", "obs_edna_taxa_name", "sites_site_code"))
})
