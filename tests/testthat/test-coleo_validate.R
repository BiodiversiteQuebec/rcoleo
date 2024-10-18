# This script tests coleo_validate.R

# Data set
dat <- data.frame(stringsAsFactors = FALSE,
                  sites_site_code = c("139_87_F01", "139_87_F01","139_87_F01","139_87_F01","139_87_F01", "139_87_F01"),
                  campaigns_type = c("insectes_sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol"),
                  landmarks_type = NA_character_,
                  samples_sample_code = c("2018-0108","2018-0108", "2018-0108","2018-0108","2018-0108","2018-0108"),
                  observations_date_obs = c(NA, "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24"),
                  campaigns_opened_at = c("2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24", "2018-05-24"),
                  observations_is_valid = c(NA, TRUE, TRUE, TRUE, TRUE, TRUE),
                  obs_species_taxa_name = c(NA, "Camponotus pennsylvanicus","Ceraticelus laetabilis","Insecta","Insecta","Trochosa terricola"),
                  obs_species_parent_taxa_name = c(NA, "Insecta","Insecta","Insecta","Insecta","Insecta"),
                  obs_species_variable = c(NA, "abondance","abondance","abondance","abondance","abondance"),
                  obs_species_value = c(NA, 2, 2, 1, 4, 1))


# Tests
test_that("coleo_validate", {

  ## Test for missing campaign_type column
  dat_test <- subset(dat, select = -c(campaigns_type))
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "V\U00E9rifiez qu'une colonne contient le type d'inventaire*")

  ## Test for multiple values within the campaign_type column
  dat_test <- dat
  dat_test$campaigns_type <- c("sol", "insectes_sol","insectes_sol","insectes_sol","insectes_sol","insectes_sol")
  testthat::expect_error(coleo_validate(dat_test),
                         regexp = "V\U00E9rifiez que toutes les valeurs de la colonne campaigns_type.*")

  ## Test that remote_sensing indicators are recognized as campaigns types (1.e., 1 entry in remote_sensing_events table)
  dat_test <- data.frame(remote_sensing_indicators_name = c("NDSI", "NDSI"),
                         cells_cell_code = c("105_101", "105_101"),
                         remote_sensing_events_date_start = c("2020-07-22", "2020-07-22"),
                         remote_sensing_obs_metric = c("max", "min"),
                         remote_sensing_obs_value = c(0.5, 0.2))
  testthat::expect_message(coleo_validate(dat_test), regexp = "*remote_sensing_events : 1*")

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
  dat_test$obs_species_parent_taxa_name[1] <- "Insecta"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez les lignes sans observation.*")

  
  ## Check that campaigns, efforts and landmarks are not duplicated by comments
  ### Campaigns_notes
  dat_test <- dat
  dat_test$campaigns_notes <- NA
  dat_test$campaigns_notes[2] <- 'Note'

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "V\u00E9rifiez les commentaires de campagnes*")
  ### efforts_notes
  dat_test <- dat
  dat_test$campaigns_type <- "acoustique_chiroptères"
  dat_test$samples_sample_code <- NULL
  dat_test$efforts_time_start <- "08:00:00"
  dat_test$efforts_notes <- NA
  dat_test$efforts_notes[2] <- 'Note'

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "V\u00E9rifiez les commentaires des efforts*")
  ### landmarks_notes
  dat_test <- dat
  dat_test$landmarks_notes <- NA
  dat_test$landmarks_notes[2] <- 'Note'

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "V\u00E9rifiez les commentaires des rep\u00E8res*")

  ## Test that all (and only) ADNe campaigns observations made at the ## station level have landmarks
  dat_test <- dat
  dat_test[, c("samples_sample_code", "obs_species_variable", "obs_species_value")] <- NULL
  dat_test$campaigns_type <- "ADNe"
  dat_test$obs_edna_taxa_name <- "Insecta"
  dat_test$obs_species_taxa_name <- NULL
  dat_test$observations_extra_variable_1 <- "level"
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
  dat_test$landmarks_type <- "inconnu"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez les valeurs contenues dans les colonnes.*")

  ## Test that non-breaking spaces are detected
  dat_test <- dat
  dat_test$obs_species_taxa_name[[2]] <- "Camponotus\u00A0pennsylvanicus"

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez la présence d'espaces insécables*")

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

  ## Test that Na values in time columns are detected
  dat_test <- dat

  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Certaines valeurs de date sont manquantes ou NA*")

  ## Test that date format respects the YYYY-MM-DD convention
  dat_test <- dat
  dat_test$observations_date_obs <- "95-05-15"
  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez le format des valeurs de dates.*")
  dat_test <- dat
  dat_test$observations_date_obs <- "2020_05_15"
  testthat::expect_warning(coleo_validate(dat_test),
                         regexp = "Vérifiez le format des valeurs de dates.*")

  ## Test that the range of dates are returned
  dat_test <- dat[-1,]

  testthat::expect_message(coleo_validate(dat_test),
                         regexp = "*Vérifiez que l'intervalle des dates injectées correspond aux attentes.*")

  ## Test that number of entries per table is correct
  testthat::expect_message(coleo_validate(dat_test),
                         regexp = "*Résumé des injections par table*")
  testthat::expect_message(coleo_validate(dat_test), regexp = "*campaigns : 1*")

  ## Test that the warning message includes the number of observations missing
  dat_test <- dat
  testthat::expect_match(missing_obs(dat_test, nvals = 6),
                         regexp = "*1 lignes sans taxon observé entraineront un \u00E9chec d'injection)*")

  ## Test that the warning message includes the number of vegetation campaigns already in coleo
  dat_test <- structure(list(campaigns_type = c("végétation_transect", "végétation_transect"),
    sites_site_code = c("141_124_H01", "141_124_H01"),
    campaigns_opened_at = c("2020-07-22", "2020-07-22"), 
    campaigns_technicians = list(c("C Lang", "E Carignan"), c("C Lang", "E Carignan")),
    efforts_samp_surf = c(100, 100),
    efforts_samp_surf_unit = c("m2", "m2"), 
    efforts_notes = c(NA_character_, NA_character_),
    observations_date_obs = c("2020-07-22", "2020-07-22"),
    observations_stratum = c("arbustive", "arbustive"), 
    obs_species_taxa_name = c("Ilex mucronata", "Picea mariana"),
    obs_species_variable = c("catégorie_recouvrement", "catégorie_recouvrement"),
    obs_species_value_string = c("2", "3"),
    observations_notes = c(NA_character_, NA_character_), 
    obs_species_parent_taxa_name = c("Plantae", "Plantae")), 
  row.names = 1:2,
  class = "data.frame")

  testthat::expect_match(new_vegetation_transect_campaigns(dat_test, nvals = 6),
                         regexp = "*campagnes existent d\u00E9j\u00E0 dans coleo)*")
  
})















