#############################################
# Params
#############################################

# Function to generate a random cell
random_cell <- function() {

	# Define a set of words for the theme
	theme_words <- c("hobbit", "elf", "dwarf", "wizard", "orc")
	# Define a set of suffixes to add to the theme word
	suffixes <- c("shire", "wood", "mountain", "forge", "keep")
	# Define a set of prefixes to add to the theme word
	prefixes <- c("Bilbo", "Gandalf", "Frodo", "Legolas", "Gimli")

	# Generate a random name based on the theme
	random_name <- paste0(sample(prefixes, 1), "'s ", sample(theme_words, 1), " of the ", sample(suffixes, 1))

	# Generate a random cell code
	cell_code <- paste0(paste0(sample(LETTERS, 3), collapse = ""), "_", 
						paste0(sample(LETTERS, 3), collapse = ""))
	# Generate a random name
	name <- paste0(sample(prefixes, 1), "'s ", sample(theme_words, 1), " of the ", sample(suffixes, 1))
	# Generate geometry
	geom <- list(type = "Polygon", 
				coordinates = list(
							list(c(-79.340288, 48.511171),
								c(-79.451258, 48.4084),
								c(-79.5175150698258, 48.43971636114),
								c(-79.5176355352339, 48.5000133539952),
								c(-79.517581626098, 48.503401649012),
								c(-79.5175785798052, 48.5615006339734),
								c(-79.5175785437023, 48.5643904071127)))
	) # Return the coordinates as a list

	# Return a tibble
	return(tibble::tibble(cell_code = cell_code,
							name = name,
							geom = list(geom)))
}


#############################################
# Test coleo_inject
#############################################

# Test that coleo_inject returns an output dataframe
test_that("coleo_inject returns an output dataframe", {
 		output <- coleo_inject(random_cell(), schema = "coleo_test")
 	expect_s3_class(output, "data.frame")
})

# Test that coleo_inject performs a successful injection
test_that("coleo_inject performs a successful injection", {
  out <- coleo_inject(random_cell(), schema = "coleo_test")
  expect_null(out$cell_error[[1]])
})

# Expect one dataframe with two row being returned
test_that("coleo_inject injects all data", {

	# Mock data
	acoustique_data <- structure(list(
			sites_site_code = c("139_87_H01", "139_87_H01"), campaigns_type = c("acoustique_chiroptères", "acoustique_chiroptères"), landmarks_lat = c(
				45.00642,
				45.00642
			),
			landmarks_lon = c(rnorm(1, mean = -73.81944, sd = 0.5), rnorm(1, mean = -73.81944, sd = 0.5)),
			campaigns_opened_at = c("2018-04-24", "2018-04-24"), campaigns_closed_at = c("2018-04-25", "2018-04-25"),
			devices_mic_ultra_code = c("SM3-42-5742", "SM3-42-5742"),
			obs_species_taxa_name = c("Lasionycteris noctivagans", "Myotis lucifugus | Myotis septentrionalis | Myotis leibii"),
			observations_is_valid = c(TRUE, TRUE),
			observations_date_obs = c("2018-04-24", "2018-04-24"), obs_species_variable = c("présence", "présence"),
			observations_time_obs = c("21:54:09", "21:47:31"),
			efforts_time_start = c("19:52:30", "19:52:30"), efforts_time_finish = c("00:52:30", "00:52:30"),
			efforts_recording_minutes = c(300L, 300L)
			), 
		row.names = 1:2, class = "data.frame")

	# Perform injection
	out_inject <- coleo_inject(acoustique_data, schema = "coleo_test")

	# Expect a dataframe with two rows
	expect_equal(nrow(out_inject), 2)

	# Expect the right columns
	expect_named(out_inject, c("campaign_id", "device_id", "site_id", "effort_id", "observations_efforts_lookup_id", 
                            "landmark_id", "observations_landmarks_lookup_id", "obs_specie_id", 
                            "observation_id", "campaign_error", "device_error", "effort_error", 
                            "landmark_error", "observation_error", "observations_efforts_lookup_error", 
                            "observations_landmarks_lookup_error", "obs_specie_error", "campaigns_closed_at", 
                            "campaigns_opened_at", "campaigns_type", "devices_mic_ultra_code", 
                            "efforts_recording_minutes", "efforts_time_finish", "efforts_time_start", 
                            "landmarks_geom", "observations_date_obs", "observations_is_valid", 
                            "observations_time_obs", "sites_site_code", "obs_species_taxa_name", 
                            "obs_species_variable"))
})


#############################################
# Test coleo_inject_general
#############################################

test_inject_general <- function(){
  # Call the function with test data
  test_cell <- random_cell()

  # Prepare the request
  demo_test <- coleo_inject_general(cell_code = test_cell$cell_code,
                                    name = test_cell$name,
                                    endpoint = "cells",
                                    geom = test_cell$geom[[1]],
                                    schema = "coleo_test")  

  # Return the result
  return(demo_test)
}

# Define the test for coleo_inject_general
test_that("coleo_inject_general sends a valid request body", {
  # Call the function with test data
  demo_test <- test_inject_general()
  # Perform the request
  demo_result <- httr2::req_perform(demo_test)

  # Check that the request body is a named list
  expect_equal(names(demo_test$body$data), c("cell_code", "name", "geom"))
  expect_equal(names(demo_test$body$data$geom), c("type", "coordinates"))
  expect_type(demo_test$body$data, "list")
})


#############################################
# Test coleo_extract_id
#
# An integer value represented the generated
# id is expected upon successful injection
#############################################

test_that("ID for new record is a number", {
  # Call the function with test data
  demo_test <- test_inject_general()
  # Perform the request
  demo_result <- httr2::req_perform(demo_test)

  expect_gt(coleo_extract_id(demo_result), 1L)
})


#############################################
# Test coleo_inject_general_df
#############################################

test_that("rowwise approach on a data frame yields the same output as a hand-crafted request", {
  ## create -- but don't perform -- the same request
  test_cell <- random_cell()
  injection_df <- tibble::tibble(cell_code = test_cell$cell_code,
                                name = test_cell$name,
                                geom = test_cell$geom)

  demo_test <- coleo_inject_general(cell_code = test_cell$cell_code,
                                    name = test_cell$name,
                                    endpoint = "cells",
                                    geom = test_cell$geom[[1]],
                                    schema = "coleo_test")

  one_post_in_df <- injection_df |>
    dplyr::rowwise() |>
    dplyr::mutate(req = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "cells")))

  # Test
  expect_equivalent(unlist(demo_test$body), unlist(one_post_in_df$req[[1]]$body))
})


#############################################
# Test coleo_injection_execute
#############################################

test_that("coleo_injection_execute returns an error when request columns are duplicated", {
 test_cell <- random_cell()
  injection_df <- tibble::tibble(cell_code = test_cell$cell_code,
                                name = test_cell$name,
                                geom = test_cell$geom)

  demo_test <- coleo_inject_general(cell_code = test_cell$cell_code,
                                    name = test_cell$name,
                                    endpoint = "cells",
                                    geom = test_cell$geom[[1]],
                                    schema = "coleo_test")

  one_post_in_df <- injection_df |>
    dplyr::rowwise() |>
    dplyr::mutate(req = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "cells")))

  dup_requests <- one_post_in_df
  dup_requests$req2 <- dup_requests$req

  expect_error(coleo_injection_execute(dup_reqests))
})

test_that("coleo_injection_execute returns the correct response", {
  # Prepare the request
  one_post_prep <- random_cell() |>
    dplyr::rowwise() |>
    dplyr::mutate(req = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "cells", schema = "coleo_test")))
  
  # Perform the request
  one_post_response <- one_post_prep |> coleo_injection_execute()

  # Tests
  expect_true(all(c("result", "error", "success") %in% names(one_post_response)))
  expect_s3_class(one_post_response$result[[1]], "httr2_response")
  expect_equal(one_post_response$success, TRUE)
})


#############################################
# Test coleo_injection_prep
#############################################

test_that("coleo_injection_prep formats requests as an httr2 request", {

  fake_land <- tibble::tribble(
    ~campaign_id, ~trap_id, ~landmark_id, ~observation_date, ~observation_is_valid, ~sample_code, ~ref_taxa_rank, ~ref_taxa_tsn, ~ref_taxa_name, ~observation_taxa_name, ~observation_variable, ~observation_value, ~observation_notes,
    862L,      49L,         614L,      "2020-06-30",                  TRUE,  "2020-0097",  "sous-classe",            NA, "Fake_beetleA",         "Fake_beetleA",           "abondance",                  1,                 NA,
    862L,      49L,         614L,      "2020-06-30",                  TRUE,  "2020-0097",       "espèce",            NA, "Fake_beetleB",         "Fake_beetleB",           "abondance",                  6,                 NA,
    862L,      49L,         614L,      "2020-06-30",                  TRUE,  "2020-0097",       "espèce",            NA, "Fake_beetleC",         "Fake_beetleC",           "abondance",                 10,                 NA,
    862L,      49L,         614L,      "2020-06-30",                  TRUE,  "2020-0097",       "espèce",            NA, "Fake_beetleR",         "Fake_beetleR",           "abondance",                 11,                 NA,
    862L,      50L,         615L,      "2020-06-30",                  TRUE,  "2020-0098",       "classe",            NA, "Fake_beetleA",         "Fake_beetleA",           "abondance",                  1,                 NA,
    862L,      50L,         615L,      "2020-06-30",                  TRUE,  "2020-0098",       "classe",            NA, "Fake_beetleG",         "Fake_beetleG",           "abondance",                  1,                 NA,
    862L,      50L,         615L,      "2020-06-30",                  TRUE,  "2020-0098",       "espèce",            NA, "Fake_beetleH",         "Fake_beetleH",           "abondance",                  1,                 NA,
    863L,      51L,         616L,      "2020-07-21",                  TRUE,  "2020-0105",  "sous-classe",            NA, "Fake_beetleL",         "Fake_beetleL",           "abondance",                  3,                 NA
  )

  formatted_injections <- fake_land |> coleo_injection_prep("samples", schema = "coleo_test")

  expect_s3_class(formatted_injections$inject_request[[1]], "httr2_request")

  # could also test content
})


#############################################
# Test coleo_injection_final
#############################################

test_that("finalizing function extracts the right thing", {
  # Prepare the request
  one_post_prep <- random_cell() |>
    dplyr::rowwise() |>
    dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "cells", schema = "coleo_test")))
  
  # Perform the request
  response_from_api <- one_post_prep |> coleo_injection_execute()

	# Estract ID and error messages from response
  finalized_injection <- response_from_api |> coleo_injection_final()


   # Expect the new sample_id column
   expect_named(finalized_injection, c("cell_id", "cell_error", "cells_cell_code", "cells_geom", "cells_name"))
})


#############################################
# Test coleo_injection_table
#############################################

test_that("coleo_inject_table return a data.frame and adds an id column", {
  # Mock data
  acoustique_data <- structure(list(
    sites_site_code = c("139_87_H01", "139_87_H01"), campaigns_type = c("acoustique_chiroptères", "acoustique_chiroptères"), landmarks_lat = c(
      45.00642,
      45.00642
    ),
    landmarks_lon = c(-73.81944, -73.81944),
    campaigns_opened_at = c("2018-04-24", "2018-04-24"), campaigns_closed_at = c("2018-04-25", "2018-04-25"),
    devices_mic_ultra_code = c("SM3-42-5742", "SM3-42-5742"),
    obs_species_taxa_name = c("Lasionycteris noctivagans", "Myotis lucifugus | Myotis septentrionalis | Myotis leibii"),
    observations_is_valid = c(TRUE, TRUE),
    observations_date_obs = c("2018-04-24", "2018-04-24"), obs_species_variable = c("présence", "présence"),
    observations_time_obs = c("21:54:09", "21:47:31"),
    efforts_time_start = c("19:52:30", "19:52:30"), efforts_time_finish = c("00:52:30", "00:52:30"),
    efforts_recording_minutes = c(300L, 300L)
  ), row.names = 1:2, class = "data.frame")

	# Inject campaigns table
	df_out <- coleo_inject_table(acoustique_data, "campaigns", schema = "coleo_test")
	
	expect_s3_class(df_out, class = "data.frame")
	expect_named(df_out, c('campaign_id', 'site_id', 'campaign_error', 'campaigns_closed_at', 'campaigns_opened_at', 'campaigns_type', 'sites_site_code', 'landmarks_lat', 'landmarks_lon', 'devices_mic_ultra_code', 'obs_species_taxa_name', 'observations_is_valid', 'observations_date_obs', 'obs_species_variable', 'observations_time_obs', 'efforts_time_start', 'efforts_time_finish', 'efforts_recording_minutes'))
})


#############################################
# Test coleo_inject_mam_landmarks
#############################################

test_mam_injection <- function() {
  # Mock data
  data_mam <- structure(list(
    campaigns_type = c("mammifères", "mammifères"),
    sites_site_code = c("97_90_F01", "97_90_F01"),
    campaigns_opened_at = c("2020-06-29", "2020-06-29"),
    campaigns_closed_at = c("2020-10-22", "2020-10-22"),
    campaigns_technicians = list("I Dumais", "I Dumais"),
    campaigns_notes = c("time lapse", "mouvement"),
    efforts_recording_minutes = c(1000L, NA),
    efforts_photo_count = c(NA, 1001L),
    efforts_time_start = c("00:00:01", NA),
    efforts_time_finish = c("23:59:59", "23:59:59"),
    efforts_notes = c(NA_character_, NA_character_),
    devices_sd_card_codes = list(NA_character_, NA_character_),
    devices_cam_code = c("R249-01", "R249-01"),
    devices_cam_h_cm = c(152, 152),
    landmarks_lat_camera = c(48.4367, 48.4367),
    landmarks_lon_camera = c(-79.427055, -79.427055),
    landmarks_tree_code_camera = c(NA_character_, NA_character_),
    landmarks_taxa_name_camera = c("Malus", "Malus"),
    landmarks_dbh_camera = c(850L, 850L),
    landmarks_azimut_camera = c(95L,95L),
    landmarks_notes_camera = c(NA_character_, NA_character_),
    landmarks_type_camera = c("gps", "gps"),
    lures_installed_at = c("2020-06-29", "2020-06-29"),
    lures_lure = c("Sardines et leurre LDC", "Sardines et leurre LDC"),
    landmarks_lat_appat = c(48.4367, 48.4367),
    landmarks_lon_appat = c(-79.42699, -79.42699),
    landmarks_tree_code_appat = c(NA_character_, NA_character_),
    landmarks_taxa_name_appat = c("Malus", "Malus"),
    landmarks_dbh_appat = c(850L, 850L),
    landmarks_azimut_appat = c(0L,0L),
    landmarks_distance_appat = c(620, 620),
    landmarks_distance_unit_appat = c("mm", "mm"),
    landmarks_type_appat = c("gps", "gps"),
    landmarks_notes_appat = c(NA_character_, NA_character_),
    observations_date_obs = c("2020-06-30", "2020-06-30"),
    observations_time_obs = c("13:01:01", "18:01:01"),
    observations_is_valid = c(TRUE, TRUE),
    observations_note = c(NA_character_, NA_character_),
    observations_extra_variable_1 = c("degre_certitude", "degre_certitude"),
    observations_extra_value_1 = c("élevé", "élevé"),
    observations_extra_type_1 = c("character", "character"),
    observations_extra_description_1 = c(
      "Degré de certitude de l'identification",
      "Degré de certitude de l'identification"
    ),
    obs_species_taxa_name = c("Lepus americanus", "Lepus americanus"),
    obs_species_variable = c("abondance", "abondance"),
    obs_species_value = c(1, 1),
    media_type = c("image", "image"),
    media_og_format = c("jpg", "jpg"),
    media_og_extention = c(".jpg", ".jpg"),
    media_name = c(
      "97_90_F01_R439-03_20200612_004813.JPG",
      "97_90_F01_R439-03_20200612_004814.JPG"
    )
  ),
  row.names = c(NA, -2L),
  class = "data.frame")

	# Perform injection
	df_camp <- coleo_inject_table(data_mam, "campaigns", schema = "coleo_test")
	df_lures <- coleo_inject_table(df_camp, "lures", schema = "coleo_test")
	df_cam <- coleo_inject_table(df_lures, "devices", schema = "coleo_test")
	df_obs <- coleo_inject_table(df_lures, "observations", schema = "coleo_test")
	df_id <- coleo_inject_mam_landmarks(df_obs, schema = "coleo_test")

  # Return the data.frame
  return(df_id)
}


# Check injection
# - a data.frame is a list
test_that("injection of landmarks return a data.frame", {
  # Perform injection
  df_id <- test_mam_injection()
	
	# Check that the output is a data.frame
	expect_type(df_id, "list")
})

# Check that the data.frame contains lure_ids and lure_errors
test_that("the data.frame contains lure_ids and lure_errors", {
  # Perform injection
  df_id <- test_mam_injection()

  expect_named(df_id,
    c("campaign_id", "landmark_camera_id", "landmark_lure_id", "lure_id", 
    "observation_id", "site_id", "campaign_error", "landmark_camera_error", 
    "landmark_lure_error", "lure_error", "observation_error", "campaigns_closed_at", 
    "campaigns_notes", "campaigns_opened_at", "campaigns_technicians", 
    "campaigns_type", "devices_cam_code", "devices_cam_h_cm", "devices_sd_card_codes", 
    "efforts_notes", "efforts_photo_count", "efforts_recording_minutes", 
    "efforts_time_finish", "efforts_time_start", "landmarks_azimut_appat", 
    "landmarks_azimut_camera", "landmarks_dbh_appat", "landmarks_dbh_camera", 
    "landmarks_distance_appat", "landmarks_distance_unit_appat", 
    "landmarks_geom_appat", "landmarks_geom_camera", "landmarks_notes_appat", 
    "landmarks_notes_camera", "landmarks_taxa_name_appat", "landmarks_taxa_name_camera", 
    "landmarks_tree_code_appat", "landmarks_tree_code_camera", "landmarks_type_appat", 
    "landmarks_type_camera", "lures_installed_at", "lures_lure", 
    "media_name", "media_og_extention", "media_og_format", "media_type", 
    "obs_species_taxa_name", "obs_species_value", "obs_species_variable", 
    "observations_date_obs", "observations_extra", "observations_is_valid", 
    "observations_landmarks_lookup_error_appat", "observations_landmarks_lookup_error_camera", 
    "observations_landmarks_lookup_id_appat", "observations_landmarks_lookup_id_camera", 
    "observations_note", "observations_time_obs", "sites_site_code"
    )
  )
})


#############################################
# Test coleo_inject_vegetation_transect_campaigns
#############################################

test_veg_transect_injection <- function() {
  # Mock data
  data_vegetation_transect <- structure(list(campaigns_type = c("végétation_transect", "végétation_transect"),
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

  # Perform injection
  df_camp <- coleo_inject_vegetation_transect_campaigns(data_vegetation_transect, schema = "coleo_test")
}

# Check injection
# - a data.frame is a list
test_that("injection of vegetation_transect campaigns return a data.frame", {
  # Perform injection
  df_id <- test_veg_transect_injection()
	
	# Check that the output is a data.frame
	expect_type(df_id, "list")

  # Check that the data.frame contains a campaign_id column
  expect_true("campaign_id" %in% names(df_id))
  expect_true(all(!is.na(df_id$campaign_id)))
})