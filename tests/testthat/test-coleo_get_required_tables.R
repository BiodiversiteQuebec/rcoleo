test_that("coleo_get_required_tables makes a table", {

  nm_tbl <- coleo_get_required_tables()

  testthat::expect_s3_class(nm_tbl, "tbl_df")

  expect_equal(dput(sapply(nm_tbl, class)),
               c(table = "character", végétation = "numeric", végétation_transect = "numeric",
                 sol = "numeric", acoustique = "numeric", phénologie = "numeric",
                 mammifères = "numeric", papilionidés = "numeric", odonates = "numeric",
                 insectes_sol = "numeric", ADNe = "numeric", zooplancton = "numeric",
                 température_eau = "numeric", température_sol = "numeric", marais_profondeur_température = "numeric"
               ))

})
