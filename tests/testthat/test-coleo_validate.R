# This script tests coleo_validate.R

# Data set
dat <- data.frame(stringsAsFactors = FALSE,
                  site_code = c("139_87_F01", "139_87_F01","139_87_F01","139_87_F01","139_87_F01", "139_87_F01"),
                  campaign_type = c("insectes_sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol"),
                  sample_code = c("2018-0108","2018-0108", "2018-0108","2018-0108","2018-0108","2018-0108"),
                  observation_date = c("2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24"),
                  observation_is_valid = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                  observation_taxa_name = c("Agroeca_ornata", "Camponotus_pennsylvanicus","Ceraticelus_laetabilis","Insecta","Insecta","Trochosa_terricola"),
                  observation_variable = c("abundance", "abundance","abundance","abundance","abundance","abundance"),
                  observation_value = c(4L, 2L, 2L, 1L, 4L, 1L),
                  ref_taxa_rank = c("espèce","espèce","espèce","espèce","espèce","espèce"),
                  ref_taxa_name = c("Agroeca_ornata", "Camponotus_pennsylvanicus","Ceraticelus_laetabilis","Insecta","Insecta","Trochosa_terricola"),
                  ref_taxa_tsn = c(1:6))


# Tests
test_that("coleo_validate", {

  ## Test for missing campaign_type column
  dat_test <- subset(dat, select = -c(campaign_type))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "La colonne camp_type est manquante.*")

  ## Test for multiple values within the campaign_type column
  dat_test <- dat
  dat_test$campaign_type <- c("sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol")
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez que toutes les valeurs de la colonne campaign_type sont identiques.*")

  ## Test for the presence of a column called site_code
  dat_test <- subset(dat, select = -c(site_code))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Le jeu de données ne contient pas de colonne nommée site_code.*")

  ## Test that the imported data has all of the required columns
  dat_test <- subset(dat, select = -c(observation_date))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Le jeu de données ne contient pas toutes les colonnes requises pour être injecté.*")

  ## test that all input columns are valid column names
  dat_test <- dat
  dat_test$erroneous_col <- "error"
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Le jeu de données contient des colonnes au nom invalide.*")

  ## Test that all values within each column is of the right class
  dat_test <- dat
  dat_test$observation_is_valid <- as.character(dat_test$observation_is_valid)
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Le jeu de données contient des colonnes de classe invalide.*")

  ## Test that the range of values contained within input columns are valid
  dat_test <- dat
  dat_test$ref_taxa_rank <- "inconnu"
  dat_test$ref_taxa_category <- "na"

  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez les valeurs contenues dans les colonnes.*")

  ## Test that date format respects the YYYY-MM-DD convention
  dat_test <- dat
  dat_test$observation_date <- "95-05-15"

  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez le format des valeurs de dates.*")

  ## Test that the range of dates are returned
  dat_test <- dat

  testthat::expect_message(coleo_validate(dat_test),
                         regexp = "Vérifiez que l'intervalle des dates injectées correspond aux attentes.*")

})














