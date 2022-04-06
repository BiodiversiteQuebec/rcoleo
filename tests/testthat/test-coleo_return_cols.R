test_that("coleo_return_cols and coleo_return_required_cols make a table", {

  # coleo_return_cols
  nm_tbl <- coleo_return_cols("acoustique")

  expect_s3_class(nm_tbl, "data.frame")

  expect_equal(sapply(nm_tbl, class),
               c(noms_de_colonnes = "character", colonne_requise = "character", classe = "character",
                 valeurs_acceptees = "list"
               ))

  # coleo_return_required_cols
  nm_req_tbl <- coleo_return_required_cols("acoustique")

  expect_s3_class(nm_req_tbl, "data.frame")

  expect_equal(sapply(nm_req_tbl, class),
               c(noms_de_colonnes = "character", colonne_requise = "character", classe = "character",
                 valeurs_acceptees = "list"
               ))
})


test_that("coleo_return_cols(required.columns=TRUE) and coleo_return_required_cols return the same output", {

  cols <- coleo_return_cols("acoustique", required.columns=TRUE)
  req_cols <- coleo_return_required_cols("acoustique")
  expect_equal(cols, req_cols)
})
