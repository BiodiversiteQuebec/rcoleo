# This script tests coleo_validate.R

# Data set
dat <- data.frame(stringsAsFactors = FALSE,
                  site_code = c("139_87_F01", "139_87_F01","139_87_F01","139_87_F01","139_87_F01", "139_87_F01"),
                  camp_type = c("insectes_sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol"),
                  sample_code = c("2018-0108","2018-0108", "2018-0108","2018-0108","2018-0108","2018-0108"),
                  date_obs = c("2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24"),
                  obs_is_valid = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                  obs_taxa_name = c("Agroeca_ornata", "Camponotus_pennsylvanicus","Ceraticelus_laetabilis","Insecta","Insecta","Trochosa_terricola"),
                  obs_variable = c("abundance", "abundance","abundance","abundance","abundance","abundance"),
                  obs_value = c(4L, 2L, 2L, 1L, 4L, 1L),
                  ref_taxa_rank = c("espèce","espèce","espèce","espèce","espèce","espèce"),
                  ref_taxa_name = c("Agroeca_ornata", "Camponotus_pennsylvanicus","Ceraticelus_laetabilis","Insecta","Insecta","Trochosa_terricola"),
                  ref_taxa_category = c("arthropodes","arthropodes","arthropodes","arthropodes","arthropodes","arthropodes"),
                  ref_taxa_tsn = c(1:6))


# Tests
test_that("coleo_validate", {

  ## Test for missing camp_type column
  dat_test <- subset(dat, select = -c(camp_type))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez qu'une colonne contient le type de campagne.*")

  ## Test for multiple values within the camp_type column
  dat_test <- dat
  dat_test$camp_type <- c("sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol")
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez que toutes les valeurs de la colonne camp_type sont identiques.*")

  ## Test for the presence of a column called site_code
  dat_test <- subset(dat, select = -c(site_code))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez qu'une colonne contient le code du site.*")

  ## Test that the imported data has all of the required columns
  dat_test <- subset(dat, select = -c(date_obs))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez que les bons noms de colonnes sont utilisés.*")

  ## test that all input columns are valid column names
  dat_test <- dat
  dat_test$erroneous_col <- "error"
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez que les bons noms de colonnes sont utilisés et que les colonnes superflues sont retirées.*")

  ## Test that all values within each column is of the right class
  dat_test <- dat
  dat_test$obs_is_valid <- as.character(dat_test$obs_is_valid)
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez la classe des colonnes.*")

  ## Test that the range of values contained within input columns are valid
  dat_test <- dat
  dat_test$ref_taxa_rank <- "inconnu"
  dat_test$ref_taxa_category <- "na"
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez les valeurs contenues dans les colonnes.*")

})















