# This script tests coleo_validate.R

# Data set
dat <- data.frame(stringsAsFactors = FALSE,
                  sites_site_code = c("139_87_F01", "139_87_F01","139_87_F01","139_87_F01","139_87_F01", "139_87_F01"),
                  campaigns_type = c("insectes_sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol"),
                  samples_sample_code = c("2018-0108","2018-0108", "2018-0108","2018-0108","2018-0108","2018-0108"),
                  observations_date_obs = c(NA, "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24"),
                  campaigns_opened_at = c("2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24"),
                  observations_is_valid = c(NA, TRUE, TRUE, TRUE, TRUE, TRUE),
                  obs_species_taxa_name = c(NA, "Camponotus pennsylvanicus","Ceraticelus laetabilis","Insecta","Insecta","Trochosa terricola"),
                  obs_species_variable = c(NA, "abondance","abondance","abondance","abondance","abondance"),
                  obs_species_value = c(NA, 2, 2, 1, 4, 1),
                  ref_species_rank = c(NA,"espèce","espèce","espèce","espèce","espèce"),
                  ref_species_name = c(NA, "Agroeca ornata | Camponotus pennsylvanicus","Ceraticelus laetabilis","Insecta","Insecta","Trochosa terricola"),
                  ref_species_tsn = c(NA, 1:5))


# Tests
test_that("coleo_validate", {

  ## Test for missing campaign_type column
  dat_test <- subset(dat, select = -c(campaigns_type))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "V\U00E9rifiez qu'une colonne contient le type de campagne.*")

  ## Test for multiple values within the campaign_type column
  dat_test <- dat
  dat_test$campaigns_type <- c("sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol")
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "V\U00E9rifiez que toutes les valeurs de la colonne campaigns_type sont identiques.*")

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

  ## Check that all sites exists in coleo
  dat_test <- dat
  dat_test$sites_site_code[1] <-  "000_000_F00"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "*000_000_F00*")

  ## Test that all campaigns without observations have all fields of 
  ## taxonomic level equal to or inferior to the observation are NA in 
  ## "empty" campaigns
  dat_test <- dat
  dat_test$ref_species_rank[1] <- "espèce"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez les lignes sans observation.*")

  ## Test that all (and only) ADNe campaigns observations made at the ## station level have landmarks
  dat_test <- dat
  dat_test[, c("samples_sample_code", "obs_species_variable", "obs_species_value")] <- NULL
  dat_test$campaigns_type <- "ADNe"
  dat_test$obs_edna_taxa_name <- "Insecta"
  dat_test$obs_species_taxa_name <- NULL
  dat_test$observations_extra_value_1 <- "lac"
  dat_test$landmarks_type <- NA_character_
  dat_test$landmarks_lat = dat_test$landmarks_lon <- 43
  
  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez les observations faites à l'échelle du lac.*")

  ## Test that variable field of obs_species table is valid (obs_species_variable column)
  dat_test <- dat
  dat_test$obs_species_variable <- "abundance"
  testthat::expect_warning(coleo_validate(dat_test),
                           regexp = "*abundance*")

  ## Test that the range of values contained within input columns are valid
  dat_test <- dat
  dat_test$ref_species_rank <- "inconnu"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez les valeurs contenues dans les colonnes.*")

  ## Check that complexes of species are correctly formated
  dat_test <- dat
  dat_test$obs_species_taxa_name[2] <- "Agroeca ornata |Camponotus pennsylvanicus"
  dat_test$ref_species_name = dat_test$obs_species_taxa_name

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez le format des complexes d'espèces :*")

  ## Test that the format of time respects the HH:MM:SS convention
  dat_test <- dat
  dat_test$observations_time_obs <- "95-05-15"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez le format des valeurs d'heure*")

  ## Test that date format respects the YYYY-MM-DD convention
  dat_test <- dat
  dat_test$observations_date_obs <- "95-05-15"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez le format des valeurs de dates.*")

  ## Test that the range of dates are returned
  dat_test <- dat

  testthat::expect_message(coleo_validate(dat_test),
                         regexp = "*Vérifiez que l'intervalle des dates injectées correspond aux attentes.*")

})















