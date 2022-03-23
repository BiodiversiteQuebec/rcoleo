# This script tests coleo_validate.R

# Data set
dat <- data.frame(stringsAsFactors = FALSE,
                  sites_site_code = c("139_87_F01", "139_87_F01","139_87_F01","139_87_F01","139_87_F01", "139_87_F01"),
                  campaigns_type = c("insectes_sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol"),
                  samples_sample_code = c("2018-0108","2018-0108", "2018-0108","2018-0108","2018-0108","2018-0108"),
                  observations_date_obs = c("2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24"),
                  campaigns_opened_at = c("2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24"),
                  observations_is_valid = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                  obs_species_taxa_name = c("Agroeca_ornata", "Camponotus_pennsylvanicus","Ceraticelus_laetabilis","Insecta","Insecta","Trochosa_terricola"),
                  obs_species_variable = c("abundance", "abundance","abundance","abundance","abundance","abundance"),
                  obs_species_value = c(4, 2, 2, 1, 4, 1),
                  ref_species_rank = c("espèce","espèce","espèce","espèce","espèce","espèce"),
                  ref_species_name = c("Agroeca_ornata", "Camponotus_pennsylvanicus","Ceraticelus_laetabilis","Insecta","Insecta","Trochosa_terricola"),
                  ref_species_tsn = c(1:6))


# Tests
test_that("coleo_validate", {

  ## Test for missing campaign_type column
  dat_test <- subset(dat, select = -c(campaigns_type))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez qu'une colonne contient le type de campagne.*")

  ## Test for multiple values within the campaign_type column
  dat_test <- dat
  dat_test$campaigns_type <- c("sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol")
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "Vérifiez que toutes les valeurs de la colonne campaigns_type sont identiques.*")

  ## Test that the imported data has all of the required columns
  dat_test <- subset(dat, select = -c(observations_date_obs))
  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez que les bons noms de colonnes sont utilisés.*")

  ## test that all input columns are valid column names
  dat_test <- dat
  dat_test$erroneous_col <- "error"
  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez que les bons noms de colonnes sont utilisés et que les colonnes superflues sont retirées.*")

  ## Test that all values within each column is of the right class
  dat_test <- dat
  dat_test$observations_is_valid <- as.character(dat_test$observations_is_valid)
  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez la classe des colonnes.*")

  ## Test that the range of values contained within input columns are valid
  dat_test <- dat
  dat_test$ref_species_rank <- "inconnu"
  dat_test$ref_species_category <- "na"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez les valeurs contenues dans les colonnes.*")

  ## Test that date format respects the YYYY-MM-DD convention
  dat_test <- dat
  dat_test$observations_date_obs <- "95-05-15"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez le format des valeurs de dates.*")

  ## Test that the range of dates are returned
  dat_test <- dat

  testthat::expect_message(coleo_validate(dat_test),
                         regexp = "Vérifiez que l'intervalle des dates injectées correspond aux attentes.*")

})















