test_that("coleo_get_required_tables_functions", {
  expect_equal(coleo_return_required_tables("insectes_sol"),
               c("sites", "campaigns", "traps", "landmarks", "samples", "observations",
                 "obs_species", "ref_species"
                 ))
})
