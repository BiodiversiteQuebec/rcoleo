with_mock_dir("inject a test site", {

  one_cell_list <- list(type = "Polygon",
                        coordinates = list(
                          list(c(-79.340288, 48.511171),
                               c(-79.451258, 48.4084),
                               c(-79.5175150698258, 48.43971636114),
                               c(-79.5176355352339, 48.5000133539952),
                               c(-79.517581626098, 48.503401649012),
                               c(-79.5175785798052, 48.5615006339734),
                               c(-79.5175785437023, 48.5643904071127))))

  demo_test <- coleo_inject_general(cell_code = "FFF_XXX",
                                    name ="Beleriad",
                                    geom = one_cell_list,
                                    endpoint = "cells")
  # demo_test$body
  # httr2::req_dry_run(demo_test)

  demo_result <- httr2::req_perform(demo_test)

  test_that("request body is a named list", {
    expect_equal(names(demo_test$body$data), c("cell_code", "name", "geom"))
    expect_equal(names(demo_test$body$data$geom), c("type", "coordinates"))
    expect_type(demo_test$body$data, "list")
  })

  test_that("ID for new record is a number", expect_gt(coleo_extract_id(demo_result), 1L))


  ## create -- but don't perform -- the same request
  injection_df <- tibble::tibble(cell_code = "FFF_XXX",
                                 name ="Beleriad",
                                 geom = list(list(one_cell_list)))

  one_post_in_df <- injection_df |>
    dplyr::rowwise() |>
    dplyr::mutate(req = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "cells")))

  test_that("rowwise approach on a data frame yields the same output as a hand-crafted request", {
    expect_equivalent(demo_test$body, one_post_in_df$req[[1]]$body)
  })

  test_that("duplicate request columns cause error", {


    dup_requests <- one_post_in_df

    dup_requests$req2 <- dup_requests$req

    expect_error(coleo_injection_execute(dup_reqests))
  })

  test_that("injection returns the correct response", {

    one_post_prep <- tibble::tibble(cell_code = "FFF_ZZZ",
                                        name ="Doriath",
                                        geom = list(list(one_cell_list))) |>
      dplyr::rowwise() |>
      dplyr::mutate(req = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "cells")))


    one_post_response <- one_post_prep |> coleo_injection_execute()


    expect_true(all(c("result", "error", "success") %in% names(one_post_response)))
    expect_s3_class(one_post_response$result[[1]], "httr2_response")
    expect_equal(one_post_response$success, TRUE)

  })

  if (FALSE) {
    # this does NOT work, because of the way that cur_data_all passes in the
    # information. It passes in the geom as a list (which we don't want)

    one_post_request <- coleo_inject_general_df(injection_df, endpoint = "cells")
  }

})


with_mock_dir("injection_prep works", {
  test_that("injection prep and processing works", {

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

    formatted_injections <- fake_land |> coleo_injection_prep("samples")

    expect_s3_class(formatted_injections$inject_request[[1]], "httr2_request")

    # could also test content



  })

  test_that("finalizing function extracts the right thing", {

   response_from_api <- structure(list(campaign_id = 862L, trap_id = 49L, landmark_id = 614L,
    sample_code = "2020-0097", data = structure(list(structure(list(
        observation_date = c("2020-06-30", "2020-06-30", "2020-06-30",
        "2020-06-30"), observation_is_valid = c(TRUE, TRUE, TRUE,
        TRUE), ref_taxa_rank = c("sous-classe", "espèce", "espèce",
        "espèce"), ref_taxa_tsn = structure(c(NA, NA, NA, NA), class = "vctrs_unspecified"),
        ref_taxa_name = c("Fake_beetleA", "Fake_beetleB", "Fake_beetleC",
        "Fake_beetleR"), observation_taxa_name = c("Fake_beetleA",
        "Fake_beetleB", "Fake_beetleC", "Fake_beetleR"), observation_variable = c("abondance",
        "abondance", "abondance", "abondance"), observation_value = c(1,
        6, 10, 11), observation_notes = structure(c(NA, NA, NA,
        NA), class = "vctrs_unspecified")), row.names = c(NA,
    -4L), class = c("tbl_df", "tbl", "data.frame"))), ptype = structure(list(
        observation_date = character(0), observation_is_valid = logical(0),
        ref_taxa_rank = character(0), ref_taxa_tsn = structure(logical(0), class = "vctrs_unspecified"),
        ref_taxa_name = character(0), observation_taxa_name = character(0),
        observation_variable = character(0), observation_value = numeric(0),
        observation_notes = structure(logical(0), class = "vctrs_unspecified")), class = c("tbl_df",
    "tbl", "data.frame"), row.names = integer(0)), class = c("vctrs_list_of",
    "vctrs_vctr", "list")), inject_request = list(structure(list(
        url = "https://coleo.biodiversite-quebec.ca/api/v1/samples",
        method = NULL, headers = list(Accept = "application/json",
            `Content-Type` = "application/json", Authorization = "Bearer df9a563471baedb26dd9af3cc927b226be6fef1a1df233d83fb0aa3f338891e0",
            useragent = "rcoleo"), body = list(data = list(campaign_id = 862L,
            trap_id = 49L, landmark_id = 614L, sample_code = "2020-0097"),
            type = "json", params = list(auto_unbox = TRUE, digits = 22,
                null = "null")), fields = list(), options = list(),
        policies = list(error_body = function (resp)
        {
            resp_json <- httr2::resp_body_json(resp)
            server_message <- resp_json$message
            error_message <- paste(resp_json$errors[[1]], collapse = ": ")
            return(paste0(server_message, ": ", error_message))
        })), class = "httr2_request")), result = list(structure(list(
        method = "POST", url = "https://coleo.biodiversite-quebec.ca/api/v1/samples",
        status_code = 201L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)",
            Date = "Sat, 12 Mar 2022 20:46:22 GMT", `Content-Type` = "application/json; charset=utf-8",
            `Content-Length` = "170", Connection = "keep-alive",
            `X-Powered-By` = "Express", `Access-Control-Allow-Origin` = "*",
            Location = "/api/v1/samples/85", ETag = "W/\"aa-9hQysUBry8WJ+WyPrh6IyPy45GQ\""), class = "httr2_headers"),
        body = as.raw(c(0x7b, 0x22, 0x69, 0x64, 0x22, 0x3a, 0x38,
        0x35, 0x2c, 0x22, 0x74, 0x72, 0x61, 0x70, 0x5f, 0x69,
        0x64, 0x22, 0x3a, 0x34, 0x39, 0x2c, 0x22, 0x73, 0x61,
        0x6d, 0x70, 0x6c, 0x65, 0x5f, 0x63, 0x6f, 0x64, 0x65,
        0x22, 0x3a, 0x22, 0x32, 0x30, 0x32, 0x30, 0x2d, 0x30,
        0x30, 0x39, 0x37, 0x22, 0x2c, 0x22, 0x75, 0x70, 0x64,
        0x61, 0x74, 0x65, 0x64, 0x5f, 0x61, 0x74, 0x22, 0x3a,
        0x22, 0x32, 0x30, 0x32, 0x32, 0x2d, 0x30, 0x33, 0x2d,
        0x31, 0x32, 0x54, 0x32, 0x30, 0x3a, 0x34, 0x36, 0x3a,
        0x32, 0x32, 0x2e, 0x33, 0x30, 0x32, 0x5a, 0x22, 0x2c,
        0x22, 0x63, 0x72, 0x65, 0x61, 0x74, 0x65, 0x64, 0x5f,
        0x61, 0x74, 0x22, 0x3a, 0x22, 0x32, 0x30, 0x32, 0x32,
        0x2d, 0x30, 0x33, 0x2d, 0x31, 0x32, 0x54, 0x32, 0x30,
        0x3a, 0x34, 0x36, 0x3a, 0x32, 0x32, 0x2e, 0x33, 0x30,
        0x32, 0x5a, 0x22, 0x2c, 0x22, 0x64, 0x61, 0x74, 0x65,
        0x5f, 0x73, 0x61, 0x6d, 0x70, 0x22, 0x3a, 0x6e, 0x75,
        0x6c, 0x6c, 0x2c, 0x22, 0x6e, 0x6f, 0x74, 0x65, 0x73,
        0x22, 0x3a, 0x6e, 0x75, 0x6c, 0x6c, 0x2c, 0x22, 0x74,
        0x72, 0x61, 0x70, 0x49, 0x64, 0x22, 0x3a, 0x34, 0x39,
        0x7d))), class = "httr2_response")), error = list(NULL),
    success = TRUE), class = c("rowwise_df", "tbl_df", "tbl",
"data.frame"), row.names = c(NA, -1L), groups = structure(list(
    campaign_id = 862L, trap_id = 49L, landmark_id = 614L, sample_code = "2020-0097",
    .rows = structure(list(1L), ptype = integer(0), class = c("vctrs_list_of",
    "vctrs_vctr", "list"))), row.names = c(NA, -1L), class = c("tbl_df",
"tbl", "data.frame")))

   finalized_injection <- response_from_api |> coleo_injection_final()


   # expect the new sample_id column
   expect_named(finalized_injection, c(
     "campaign_id", "trap_id", "landmark_id", "sample_id", "observation_date",
     "observation_is_valid", "ref_taxa_rank", "ref_taxa_tsn", "ref_taxa_name",
     "observation_taxa_name", "observation_variable", "observation_value",
     "observation_notes"
   ))

   })

})

with_mock_dir("injection of all tables works", {
  # Mock data
  data <- structure(list(
    sites_site_code = c("139_87_H01", "139_87_H01"), campaigns_type = c("acoustique", "acoustique"), landmarks_lat = c(
      45.00642,
      45.00642
    ),
    landmarks_lon = c(-73.81944, -73.81944),
    campaigns_opened_at = c("2018-04-24", "2018-04-24"), campaigns_closed_at = c("2018-04-25", "2018-04-25"),
    devices_mic_ultra_code = c("SM3-42-5742", "SM3-42-5742"),
    obs_species_taxa_name = c("Lasionycteris noctivagans", "Myotis lucifugus | Myotis septentrionalis | Myotis leibii"),
    ref_species_name = c("Lasionycteris noctivagans", "Myotis lucifugus | Myotis septentrionalis | Myotis leibii"),
    ref_species_category = c("mammifères", "mammifères"),
    observations_is_valid = c(TRUE, TRUE),
    observations_date_obs = c("2018-04-24", "2018-04-24"), obs_species_variable = c("présence", "présence"),
    observations_time_obs = c("21:54:09", "21:47:31"),
    efforts_time_start = c("19:52:30", "19:52:30"), efforts_time_finish = c("00:52:30", "00:52:30"),
    efforts_recording_minutes = c(300L, 300L)
  ), row.names = 1:2, class = "data.frame")

  # Perform injection
  out_inject <- coleo_inject(dat)

  # Expect two objects being returned
  test_that("we get a list of length two", {
    expect_length(out_inject, 2)
  })
  
  # Expect no failures
  test_that("no failures", expect_false(out_inject[["failures"]]))
  
  # Expect injection of all tables
  test_that("all tables injected", {
    expect_named(out_inject$data, c("observation_id", "obs_specie_id", "taxa_id", "campaign_id", "device_id", "landmark_id", "effort_id", "site_id", "is_valid","sites_site_code", "ref_species_name", "ref_species_category", "date_obs", "time_obs", "observation_error"))
  })
})

# Test coleo_inject_table
with_mock_dir("injection of one table", {
  df_out <- coleo_inject_table(data, "acoustique", failures = FALSE, "campaigns")


  test_that("return a data.frame", expect_s3_class(df_out, class = "data.frame"))

  test_that("id column is added", expect_named(df_out, c(
    "site_id", "campaign_id", "sites_site_code", "landmarks_lat",
    "landmarks_lon", "devices_mic_ultra_code", "obs_species_taxa_name",
    "ref_species_name", "ref_species_category", "observations_is_valid",
    "observations_date_obs", "obs_species_variable", "observations_time_obs",
    "efforts_time_start", "efforts_time_finish", "efforts_recording_minutes"
  )))
})

# Test coleo_inject_mam_lures
with_mock_dir("injection of mamifères lures", {

  # Mock data
  data_mam <- structure(list(
    site_id = c(307, 307),
    campaign_id = c(11044, 11045),
    campaigns_type = c("mammifères", "mammifères"),
    sites_site_code = c("97_90_F01", "97_90_F01"),
    campaigns_opened_at = c("2020-06-29", "2020-06-29"),
    campaigns_closed_at = c("2020-10-22", "2020-10-22"),
    campaigns_technicians = list("Isabelle Dumais", "Isabelle Dumais"),
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
    lures_installed_at_1 = c("2020-06-29", "2020-06-29"),
    lures_lure_1 = c("Sardines et leurre LDC", "Sardines et leurre LDC"),
    lures_installed_at_2 = c("2020-07-28", "2020-07-28"),
    lures_lure_2 = c("Sardines et leurre LDC", "Sardines et leurre LDC"),
    lures_installed_at_3 = c("2020-08-28", "2020-08-28"),
    lures_lure_3 = c("Sardines et leurre LDC", "Sardines et leurre LDC"),
    lures_installed_at_4 = c(NA_character_, NA_character_),
    lures_lure_4 = c(NA_character_, NA_character_),
    lures_installed_at_5 = c(NA_character_, NA_character_),
    lures_lure_5 = c(NA_character_, NA_character_),
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
    ref_species_name = c("Lepus americanus", "Lepus americanus"),
    obs_species_variable = c("abondance", "abondance"),
    obs_species_value = c(1, 1),
    ref_species_category = c("mammifères", "mammifères"),
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
  inject_ls <- coleo_inject_mam_lures(data_mam, failures = FALSE)

  # Check injection
  test_that("injection of lures return a list", expect_type(inject_ls, "list"))

  # Check that a data.frame is returned
  # - a data.frame is a list
  test_that("the list contains a data.frame", expect_type(inject_ls[['df_id']], "list"))

  # Check that the data.frame contains lure_ids
  test_that("the data.frame contains lure_ids",
    expect_named(inject_ls[["df_id"]],
      c("site_id", "campaign_id", "campaigns_type", "sites_site_code",
        "campaigns_opened_at", "campaigns_closed_at", "campaigns_technicians",
        "campaigns_notes", "efforts_recording_minutes", "efforts_photo_count",
        "efforts_time_start", "efforts_time_finish", "efforts_notes",
        "devices_sd_card_codes", "devices_cam_code", "devices_cam_h_cm",
        "landmarks_lat_camera", "landmarks_lon_camera", "landmarks_tree_code_camera",
        "landmarks_taxa_name_camera", "landmarks_dbh_camera", "landmarks_azimut_camera",
        "landmarks_notes_camera", "landmarks_type_camera", "lures_installed_at_1",
        "lures_lure_1", "lures_installed_at_2", "lures_lure_2", "lures_installed_at_3",
        "lures_lure_3", "lures_installed_at_4", "lures_lure_4", "lures_installed_at_5",
        "lures_lure_5", "landmarks_lat_appat", "landmarks_lon_appat",
        "landmarks_tree_code_appat", "landmarks_taxa_name_appat", "landmarks_dbh_appat",
        "landmarks_azimut_appat", "landmarks_distance_appat", "landmarks_distance_unit_appat",
        "landmarks_type_appat", "landmarks_notes_appat", "observations_date_obs", 
        "observations_time_obs", "observations_is_valid", "observations_note",
        "observations_extra_variable_1", "observations_extra_value_1",
        "observations_extra_type_1", "observations_extra_description_1",
        "obs_species_taxa_name", "ref_species_name", "obs_species_variable",
        "obs_species_value", "ref_species_category", "media_type", "media_og_format",
        "media_og_extention", "media_name", "lure_id_1", "lure_id_2",
        "lure_id_3"
      )
    )
  )
})

# Test coleo_inject_mam_landmarks
with_mock_dir("injection of mamifères landmarks", {

  # Perform injection
  inject_land_ls <- coleo_inject_mam_landmarks(inject_ls[['df_id']], failures = FALSE)

  # Check injection
  test_that("injection of landmarks return a list", expect_type(inject_land_ls, "list"))

  # Check that a data.frame is returned
  # - a data.frame is a list
  test_that("the landmark list contains a data.frame", expect_type(inject_land_ls[['df_id']], "list"))

  # Check that the data.frame contains lure_ids
  test_that("the data.frame contains lure_ids",
    expect_named(inject_land_ls[["df_id"]],
      c("campaign_id", "landmark_id_camera", "site_id", "campaigns_type",
        "sites_site_code", "campaigns_opened_at", "campaigns_closed_at",
        "campaigns_technicians", "campaigns_notes", "efforts_recording_minutes",
        "efforts_photo_count", "efforts_time_start", "efforts_time_finish",
        "efforts_notes", "devices_sd_card_codes", "devices_cam_code",
        "devices_cam_h_cm", "lures_installed_at_1", "lures_lure_1", "lures_installed_at_2",
        "lures_lure_2", "lures_installed_at_3", "lures_lure_3", "lures_installed_at_4",
        "lures_lure_4", "lures_installed_at_5", "lures_lure_5", "landmarks_lat_appat",
        "landmarks_lon_appat", "landmarks_tree_code_appat", "landmarks_taxa_name_appat",
        "landmarks_dbh_appat", "landmarks_azimut_appat", "landmarks_distance_appat",
        "landmarks_distance_unit_appat", "landmarks_type_appat", "landmarks_notes_appat",
        "observations_date_obs", "observations_time_obs", "observations_is_valid",
        "observations_note", "observations_extra_variable_1", "observations_extra_value_1",
        "observations_extra_type_1", "observations_extra_description_1",
        "obs_species_taxa_name", "ref_species_name", "obs_species_variable",
        "obs_species_value", "ref_species_category", "media_type", "media_og_format",
        "media_og_extention", "media_name", "lure_id_1", "lure_id_2",
        "lure_id_3", "landmark_id_appat_1", "landmark_id_appat_2", "landmark_id_appat_3"
      )
    )
  )

})
