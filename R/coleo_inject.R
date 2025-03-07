#' Injecte un jeu de données dans Coleo.
#'
#' Principale fonction d'injection de données dans Coleo. Cette fonction
#' accepte un data.frame validé par \code{\link[rcoleo]{coleo_validate}} et
#' performe automatiquement l'injection de toutes les tables dans Coleo.
#'
#' @param df Un data.frame validé par \code{\link[rcoleo]{coleo_validate}}
#' @param media_path NULL par défault. Requis lorsqu'il y a des fichiers médias
#' à injecter. Doit être le chemin local vers les fichiers médias à injecter.
#' @param schema Schéma sur lequel faire la requête. Par défaut, le schéma
#' public est utilisé.
#'
#' @return Un message de succès ou d'échec de l'injection des lignes du
#' data.frame par table et le data.frame retourné par
#' \code{\link[rcoleo]{coleo_injection_execute}}.

#' @export

coleo_inject <- function(df, media_path = NULL, schema = 'public') {
  #--------------------------------------------------------------------------
  # 0. Injection process for cells and sites
  # - data <- coleo_read_shape(fileName)
  #--------------------------------------------------------------------------
  # Cells
  # - Only cells have a geom column
  if ("geom" %in% colnames(df)) {
    if ("sfc_POLYGON" %in% class(df$geom) | "sfc_MULTIPOLYGON" %in% class(df$geom)) {
      df <- df |>
        dplyr::rowwise() |>
        ## Format cell geometry to geojson
         dplyr::mutate(geom = list(geojsonsf::sf_geojson(sf::st_sf(geom)))) |>
        tibble::as_tibble()
    }
    df_id <- coleo_inject_cells(df, schema = schema)

    return(df_id)
  }

  # Sites
  if("sites_type" %in% colnames(df)) {
    df_id <- coleo_inject_table(df, "sites", schema = schema)

    # plumber-api trigger to update portal data
    browseURL("https://coleo.biodiversite-quebec.ca/r-update-api/update/all")

    return(df_id)
  }

  #==========================================================================
  # 1. Extract tables to be injected
  #==========================================================================
  campaign_type <- coleo_return_campaign_type(df)
  tables <- coleo_return_required_tables(campaign_type)

  #==========================================================================
  # 2. Inject campaigns table
  #
  # - Campaigns table are injected differently for mammifères campaigns
  #==========================================================================
  if (campaign_type == "v\u00e9g\u00e9tation_transect") {
    # Remove observations_lookup table for vegetation campaigns that do not have efforts
    # or landmarks
    ## observations_efforts_lookup
    if (!any(grepl("efforts", names(df)))) {
      tables <- tables[!tables %in% "observations_efforts_lookup"]
    }
    ## observations_landmarks_lookup
    if (!any(grepl("landmarks", names(df)))) {
      tables <- tables[!tables %in% "landmarks"]
      tables <- tables[!tables %in% "observations_landmarks_lookup"]
    }
    df_id <- coleo_inject_vegetation_transect_campaigns(df)
  ## Indicators do not require injecting in the campaigns table
  } else if (campaign_type %in% c("phénologie_indicateur")) {
    tables <- c("void", tables)
  ## Inject campaigns
  } else if (("campaigns_type" %in% colnames(df)) & ("campaigns_opened_at" %in% colnames(df))) {
    df_id <- coleo_inject_table(df, "campaigns", schema = schema)
    if(!any(sapply(df_id$campaign_error, is.null))) {
    cat("Only data for successfully injected campaigns are injected in the next tables. These following lines failed to inject: ", paste0(which(!sapply(df_id$campaign_error, is.null)), collapse = ", "), "\n")
  }
  ## Inject remote sensing events
  } else if ("remote_sensing_events_date_start" %in% colnames(df)) {
    df_id <- coleo_inject_table(df, "remote_sensing_events", schema = schema)
    if(!any(sapply(df_id$remote_sensing_event_error, is.null))) {
    cat("Only data for successfully injected remote sensing indicators are injected in the next tables. These following lines failed to inject: ", paste0(which(!sapply(df_id$remote_sensing_indicators_error, is.null)), collapse = ", "), "\n")
    }
  }

  

  #==========================================================================
  # 3. Inject other tables
  #==========================================================================
  for (table in tables[-1]) {

    # Injection of taxa_name in ref_species table
    if (table %in% c("landmarks", "obs_species", "obs_edna")) {
      if (campaign_type != "mammif\u00e8res" & table != "landmarks") {
        # Skip if landmarks table in a mammifère campaign.
        # The landmarks need first to be extracted
        # - The operation is done in coleo_inject_mam_landmarks()
        taxa_col <- df_id[,paste0(table, "_taxa_name")] |>
          as.data.frame()
        coleo_inject_ref_species(taxa_col)
      }
    }

    # Case-specific injections
    if (campaign_type == "mammif\u00e8res" & table == "landmarks") {
      ## Landmarks table for "mammifères" campaigns
      ## - observations is injected before landmarks
      ## - observations_landmarks_lookup table is injected in this step
      df_id <- coleo_inject_table(df_id, "observations", schema = schema)
      df_id <- coleo_inject_mam_landmarks(df_id)

    } else if (campaign_type == "mammif\u00e8res" & table == "observations_landmarks_lookup") {
      ## observations_landmarks_lookup table for "mammifères" campaigns
      ## is injected in the previous step
      next

    } else if (campaign_type == "mammif\u00e8res" & table == "observations") {
      ## observations table for "mammifères" campaigns
      ## is injected before the landmarks table, in the previous step
      next

    } else if (table == "media") {
      ## The special case of media files
      ### 0. Check that path to media files is provided
      if (is.null(media_path)) stop("The local path to media files is missing. Media files and table could not be injected")
      ### 1. Inject media files into coleo
      df_id <- coleo_inject_media(df_id, server_dir = 'observation', media_path)

    } else {
      ## Regular table injections
      df_id <- coleo_inject_table(df_id, table, schema = schema)
    }
  }

  # plumber-api trigger to update portal data
  browseURL("https://coleo.biodiversite-quebec.ca/r-update-api/update/no")

  return(df_id)
}


#' Fonction générale pour l'injection dans la base de données Coleo via l'API.
#'
#' @param endpoint Nom du endpoint de l'API où injecter.
#' @param schema Schema qui contient les tables de l'appel.
#' @param ... Données à injecter. Peut contenir des valeurs NA ou NULL; elles seront retirées avant l'injection.
#'
#' @return Un objet httr2 request, prêt à être envoyé à l'API Coleo.
#' @export
coleo_inject_general <- function(endpoint, schema = 'public', ...){

  if(is.null(endpoint)) stop("Need to specify the endpoint")


  request_info <- list(...)

  # experimental! drop any NA or NULL arguments, since they do not need to be injected
  request_info <- request_info |>
    purrr::discard(.p = ~all(is.na(.x))) |>
    purrr::discard(.p = ~all(is.null(.x)))

  coleo_begin_req(schema) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_body_json(data = request_info)
}


#' Préparation de la requête httr2 pour l'injection d'une ligne d'un jeu de
#' données.
#'
#' Fonction utilisée dans \code{\link[rcoleo]{coleo_injection_prep}}
#' pour la préparation de la colonne inject_request.
#' 
#' Un processus "tidy" qui maintient la cohérence des données.
#'
#' @param df_one_row Une ligne d'un jeu de données. Peut être passée via
#' \code{\link[dplyr]{cur_data_all}}
#' @param schema Schema qui contient les tables de l'appel.
#' @param endpoint endpoint sur l'API de coleo. Correspond à la table où
#' l'injection est à faire.
#'
#' @return Une requête unique de type "HTTP POST request". La requête n'est pas
#' exécutée. Elle doit être envoyée à l'API Coleo via
#' \code{\link[httr2]{req_perform}}. Peut être testée en premier via
#' \code{\link[httr2]{req_dry_run}}.
#'
#' @export
coleo_inject_general_df <- function(df_one_row, schema = 'public', endpoint) {

  df_one_row_ls <- as.list(df_one_row)

  # if there is a nested list, flatten.
    # this is a workaround for the behaviour of rowwise() when dealing with list-columns
    # Injections in coleo will be rejected if nested lists containing a single object are flattened
    # Technicians nested lists containing more than one object need to be flattened
  if (any(names(df_one_row_ls) %in% "technicians")) {
        n_techs <- df_one_row_ls["technicians"][[1]][[1]] |> length()
    if (n_techs > 1) {
      df_one_row_ls["technicians"] <- df_one_row_ls["technicians"][[1]]
    }
  }
    if (any(names(df_one_row_ls) %in% "geom")) {
      df_one_row_ls["geom"] <- df_one_row_ls["geom"][[1]]
    }

  df_one_row_ls$data <- NULL

  # drop all NULL or NA columns
  df_one_row_ls[which(is.na(df_one_row_ls))] <- NULL


  coleo_begin_req(schema) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_body_json(data = df_one_row_ls)
}


#' Injection de données dans Coleo.
#'
#' Accepte un jeu de données qui contient une colonne de requêtes POST httr2 et
#' les exécute. Son utilisation suit celle de
#' \code{\link[rcoleo]{coleo_injection_prep}} et précède celle de
#' \code{\link[rcoleo]{coleo_injection_final}} dans le processus d'injection.
#' 
#' Fonction utilisée dans \code{\link[rcoleo]{coleo_inject_table}},
#' \code{\link[rcoleo]{coleo_inject_media}},
#' \code{\link[rcoleo]{coleo_inject_mam_landmarks}} et
#' \code{\link[rcoleo]{coleo_inject_ref_species}}.
#'
#' @param df Un data.frame avec une seule colonne qui contient la requête httr2.
#'
#' @return Le même data.frame, avec une colonne supplémentaire qui contient
#' l'objet httr2 response, le message d'erreur, si nécessaire, et le code de
#' statut. Le statut (status) est TRUE si l'injection de la ligne est réussie.
#' 
#' @export
coleo_injection_execute <- function(df){


    which_req <- which(sapply(df, \(x) class(x[[1]])) == "httr2_request")

    if(length(which_req) != 1) stop("you must have only one request column")


    df_result <- df |>
      dplyr::mutate(inject_result = list(
        purrr::safely(httr2::req_perform)(.data[[names(which_req)]])
      ))

    df_out <- df_result |>
      dplyr::mutate(result = list(inject_result$result),
             error = list(inject_result$error),
             success = is.null(error)) |>
      dplyr::select(-inject_result)

    return(df_out)
}



#' Prépare un jeu de données pour l'injection dans Coleo.
#'
#' Accepte un jeu de données et retourne un jeu de données avec une colonne
#' 'inject_request' qui contient les requêtes httr2. Sont utilisation précède celle de
#' \code{\link[rcoleo]{coleo_injection_execute}} et repose sur celle de
#' \code{\link[rcoleo]{coleo_prep_input_data}}.
#' 
#' Fonction utilisée dans \code{\link[rcoleo]{coleo_inject_table}},
#' \code{\link[rcoleo]{coleo_inject_mam_landmarks}} et
#' \code{\link[rcoleo]{coleo_inject_ref_species}}.
#'
#' @param df Un data.frame formaté par \code{\link[rcoleo]{coleo_prep_input_data}}.
#' @param db_table La table de la base de données coleo dans laquelle les données doivent être injectées.
#' @param schema Schéma sur lequel faire la requête. Par défaut, le schéma
#' public est utilisé.
#' 
#' @return Le même data.frame, avec une colonne supplémentaire qui contient la requête httr2. NOTE - le data.frame est retourné \code{rowwise} pour faciliter l'injection.
#' 
#' @export

coleo_injection_prep <- function(df, db_table, schema = 'public'){

  # prep the data by nesting unneeded columns, renaming those remaining, and adding these to a request

  # the exception is the observation table, where we should NOT nest. instead we
  # rename and make the request using the columsn which the table accepts (if
  # any of these are present)

  # unless it is observations

  if(db_table == "observations") {
    colnames_of_tbl <- coleo_get_column_names(tbl = db_table)$column_name

    df_prep <- df |>
      coleo_prep_input_data(db_table, schema = schema) |>
      dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::across(dplyr::any_of(colnames_of_tbl)), endpoint = db_table, schema = schema)))

  } else if (db_table == "ref_species") {
    # ref_species is the only table where the table name and the endpoint name are NOT THE SAME
    # here we hard-code the difference. This lets us stay with the convention of using the table name as the argument (not the endpoint name)
    df_prep <- df |>
      coleo_prep_input_data(db_table, schema = schema) |>
      dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "taxa", schema = schema)))
  } else {
    df_prep <- df |>
      coleo_prep_input_data(db_table, schema = schema) |>
      dplyr::mutate(inject_request = list(coleo_inject_general_df(dplyr::cur_data_all(), endpoint = db_table, schema = schema)))

  }

  return(df_prep)

}

#' Finalise l'injection dans Coleo.
#'
#' Cette fonction extrait les nouveaux ID et les messages d'erreur des réponses
#' des injections de coleo. Les ID et erreurs sont sauvés dans une colonne nommée selon la
#' convention 'table_champ' dans le jeu de données d'origine.
#'
#' Son utilisation suit celle de \code{\link[rcoleo]{coleo_injection_execute}}
#' dans le processus d'injection.
#'
#' La fonction est utilisée dans \code{\link[rcoleo]{coleo_inject_table}},
#' \code{\link[rcoleo]{coleo_inject_media}} et
#' \code{\link[rcoleo]{coleo_inject_mam_landmarks}}
#'
#' @param df Une data.frame produit par \code{coleo_injection_execute}
#'
#' @return Un data.frame. Sans les groupes et les données imbriquées.
#'
#' @export

coleo_injection_final <- function(df) {
  #--------------------------------------------------------------------------
  # 1. Get the name of the type of table just injected and make a name_id out of it
  #--------------------------------------------------------------------------
  tbl_name <- df$inject_request[[1]] |>
    httr2::req_dry_run(quiet = TRUE) |>
    purrr::pluck("path") |>
    basename()
  # Media is a special case, because the POST is done on a server rather than on a table
  # - The injection in media table and obs_media are automated
  # - The table taxa has a different endpoint name (ref_species)
  if (!is.na(suppressWarnings(as.numeric(tbl_name)))) tbl_name <- "media"
  newname <- sub(tbl_name, pattern = "s$", replacement = "")
  if(tbl_name == "taxa") tbl_name = newname = "ref_species"

  name_id <- paste0(newname, "_id")
  name_err <- paste0(newname, "_error")

  #--------------------------------------------------------------------------
  # 2. Store id and error to new columns
  #--------------------------------------------------------------------------
  df_id <- df |>
    dplyr::mutate(!!name_id := ifelse(is.null(.data$error),
                                       coleo_extract_id(result),
                                       NA_integer_)
    ) |>
    dplyr::mutate(!!name_err := list(error))

  #--------------------------------------------------------------------------
  # 3. Rename table columns to keep
  #--------------------------------------------------------------------------
  # Select column names to keep
  old_names <- coleo_get_column_names(tbl_name)$column_name
  old_names <- old_names[!grepl("_id", old_names)]
  which_old <- names(df_id) %in% old_names
  # Make new column names
  new_names <- paste0(tbl_name, "_", names(df_id)[which_old])
  # Change column names to new names
  names(df_id)[which_old] <- new_names

  #--------------------------------------------------------------------------
  # 4. Remove the columns we don't want to keep and unnest the data
  #--------------------------------------------------------------------------
  df_out <- df_id[,order(colnames(df_id))] |>
    dplyr::ungroup() |>
    dplyr::select(-success, -inject_request, -error, -result)
  if("data" %in% colnames(df_out)) {
    df_out <- df_out |>
      tidyr::unnest(cols = c(data))
  }
  df_out <- df_out |>
    dplyr::relocate(dplyr::ends_with("_error")) |>
    dplyr::relocate(dplyr::ends_with("_id"))

  return(df_out)
}


# Helper function to return stadardized injection reponse messages
injection_reponse_message <- function(table, response) {
  resp <- table(response$success)
  # Output
  cat(paste0("\nInjection of ",table, " table led to ", ifelse(is.na(resp['TRUE']), 0, resp['TRUE']), " successes, and ", ifelse(is.na(resp['FALSE']), 0, resp['FALSE']), " failures.\n"))

  if(!is.na(resp['FALSE'])) {
    cat("These lines failed to inject: ", dput(which(response$success == FALSE)), "\n")
    ## It is expected that not all ref_species are injected
    ## - Each taxon is only injected once
    }
}


#' Injection des données dans une table de Coleo.
#'
#' Accepte un data.frame validé par \code{\link[rcoleo]{coleo_validate}} et
#' performe l'injection dans la table de coleo spécifiée.
#'
#' Cette fonction est appelée par \code{\link[rcoleo]{coleo_inject}} et fait
#' appel aux fonctions de préparation, d'exécution et de finalisation de
#' l'inejction : \code{\link[rcoleo]{coleo_injection_prep}},
#' \code{\link[rcoleo]{coleo_injection_execute}} et
#' \code{\link[rcoleo]{coleo_injection_final}}.
#'
#' @param df_id Un data.frame validé par \code{\link[rcoleo]{coleo_validate}}
#' et contenant les id des lignes injectées dans la table campaigns.
#' @param table Une table de coleo où inecter les données.
#' @param schema Schéma sur lequel faire la requête.
#'
#' @return Le même data.frame, avec les id des lignes injectées dans la table
#' et une colonne pour les erreurs.
#'
coleo_inject_table <- function(df_id, table, schema = 'public') {
  #--------------------------------------------------------------------------
  # 1. Prep request
  #--------------------------------------------------------------------------
  requests <- df_id |>
      coleo_injection_prep(db_table = table, schema = schema)

  #--------------------------------------------------------------------------
  # 2. Requests executions
  #--------------------------------------------------------------------------
  response <- coleo_injection_execute(requests) # Real thing

  #--------------------------------------------------------------------------
  # 3. Print output
  #--------------------------------------------------------------------------
  # Output
  injection_reponse_message(table, response)

  #--------------------------------------------------------------------------
  # 4. Get id
  # - Failure will cause an error
  # - Only get id for successful injections
  # - There is no id for lookup tables
  #--------------------------------------------------------------------------
  df_out <- response |>
    coleo_injection_final()

  return(df_out)
}


#' Injection des cellules dans Coleo.
#'
#' Accepte un data.frame validé par \code{\link[rcoleo]{coleo_validate}} et
#' performe l'injection dans la table cells.
#'
#' Cette fonction fait appel aux fonctions de préparation, d'exécution et de
#' finalisation de l'injection : \code{\link[rcoleo]{coleo_inject_general_df}},
#' \code{\link[rcoleo]{coleo_injection_execute}} et
#' \code{\link[rcoleo]{coleo_injection_final}}.
#'
#' @param df Un data.frame contenant une colonne `geom` de type polygon, cell_code et name.
#' @param schema Schéma sur lequel faire la requête. Par défaut, le schéma
#' public est utilisé.
#'
#' @export
coleo_inject_cells <- function(df, schema = 'public') {
  #--------------------------------------------------------------------------
  # 1. Prep request
  #--------------------------------------------------------------------------
  requests <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(inject_request = list(
      coleo_inject_general_df(dplyr::cur_data_all(), endpoint = "cells", schema = schema)
    ))
  #--------------------------------------------------------------------------
  # 2. Requests executions
  #--------------------------------------------------------------------------
  response <- coleo_injection_execute(requests) # Real thing
  #--------------------------------------------------------------------------
  # 3. Print output
  #--------------------------------------------------------------------------
  # Output
  injection_reponse_message("cells", response)
  #--------------------------------------------------------------------------
  # 4. Get id
  # - Failure will cause an error
  # - Only get id for successful injections
  #--------------------------------------------------------------------------
  df_out <- response |>
    coleo_injection_final()

  return(df_out)
}


#' Injection des fichiers média dans le serveur de Coleo.
#'
#' L'injection des fichiers médias se fait hors de la base de données coleo, mais sur le serveur. dépendamment du type de fichiers médias, l'injection se fait dans un dossier spécifique : soit dans le dossier \code{observation} soit dans le dossier \code{campaign}, ou soit sans le dossier \code{site}.
#'
#' L'injection des fichiers médias sur le serveur de coleo engendre
#' automatiquement l'injection des données dans la table \code{media} de coleo. Il n'est donc pas requis d'injecter la table \code{media} manuellement.
#'
#' Il faut spécifier le id correspondant au dossier \code{site} dans lequel les fichiers médias seront injectés. Par enxemple, une injection dans le fichier campaigns doit spécifier le id de la campagne, tandis qu'une injection dans le dossier observation doit spécifier le id de l'observation. Le id permet la sauvegarde du fichier média dans un sous-dossier spécifique.
#'
#' La fonction est utilisée par \code{\link[rcoleo]{coleo_inject}}.
#'
#' @param df_id Un data,frame contenant les colonnes *_id nécessaires.
#' @param server_dir Le nom du dossier dans lequel les fichiers médias seront
#' injectés (campaign, observation, site).
#' @param file_dir Le chemin local vers le dossier contenant les fichiers médias à injecter.
#'
#' @return Le même data.frame, avec les id des lignes injectées dans la table media.
#'
#' @export
coleo_inject_media <- function(df_id, server_dir = "observation", file_dir) {
  #--------------------------------------------------------------------------
  # 1. Prep request
  #--------------------------------------------------------------------------
  medias_requests <- df_id |>
    dplyr::rowwise() |>
    dplyr::mutate(inject_request = list(
      ## Form request
      coleo_media_begin_req(server_dir) |>
      ## Add id to request URL
      ## - Can be observation_id, campaign_id, etc.
      httr2::req_url_path_append(!!as.name(paste0(server_dir, "_id"))) |>
      ## Send file
      httr2::req_body_multipart(
        media = curl::form_file(paste0(file_dir, "/", media_name)),
        type = "image")
    ))

  #--------------------------------------------------------------------------
  # 2. Injection
  #--------------------------------------------------------------------------
  response <- coleo_injection_execute(medias_requests)

  #--------------------------------------------------------------------------
  # 3. Print output
  #--------------------------------------------------------------------------
  # Output
  injection_reponse_message("media files and media", response)

  #--------------------------------------------------------------------------
  # 4. Get id back
  # - From the response, get the id of the media file
  #--------------------------------------------------------------------------
  df_id <- response |>
    coleo_injection_final()

  return(df_id)
}


#' Injection des repères d'une campagne mammifères dans la table
#' landmarks de coleo.
#'
#' L'injection des repères pour les campagnes mammifères est différente
#' puiqu'il y a pour chaque observation un repère pour la caméra et un autre 
#' pour le piège. Nous procédons donc à l'injection successif de tous les
#' repères.
#'
#' Accepte un data.frame validé par \code{\link[rcoleo]{coleo_validate}} et
#' performe l'injection de la table landmarks.
#' 
#' La fonction est utilisée par \code{\link[rcoleo]{coleo_inject}}.
#'
#' @param df_id Un data.frame contenant une colonne campaign_id.
#' @param schema Schéma sur lequel faire la requête. Par défaut, le schéma
#' public est utilisé.
#'
#' @return Une data.frame avec une colonne landmark_X_id et une colonne pour
#' les landmark_X_error.
#'
coleo_inject_mam_landmarks <- function(df_id, schema = 'public') {
  #--------------------------------------------------------------------------
  # 1. Inject taxa names in ref_species table for camera landmark
  #--------------------------------------------------------------------------
  if ("landmarks_taxa_name_camera" %in% names(df_id)) {
    coleo_inject_ref_species(df_id[,"landmarks_taxa_name_camera"])
  }
  #--------------------------------------------------------------------------
  # 2. Inject camera landmark
  #--------------------------------------------------------------------------
  # Remove `_camera` from colnames
  df_camera <- df_id
  names(df_camera)[grep("_camera", names(df_camera))] <- sub('_camera', "\\1", names(df_camera)[grep("_camera", names(df_camera))], perl = TRUE)
  # Hide `lure_id` column name
  names(df_camera)[names(df_camera) == "lure_id"] <- "lure_id_appat"
  # Prep requests
  requests <- df_camera |>
    coleo_injection_prep(db_table = "landmarks", schema = schema)
  # Injection
  response <- coleo_injection_execute(requests) # Real thing
  # Output
  injection_reponse_message("landmark_camera", response)
  # Finalizing landmarks table injection
  df_id <- response |>
    coleo_injection_final()
  
  # Inject camera landmark into observation_landmarks_lookup table
  df_id <- coleo_inject_table(df_id, "observations_landmarks_lookup", schema = schema)

  # Bring back `lure_id` column name
  names(df_id)[names(df_id) == "lure_id_appat"] <- "lure_id"

  # Rename camera columns
  which_camera <- grepl("landmarks", names(df_id)) & !grepl("_appat", names(df_id))
  names(df_id)[which_camera] <- paste0(names(df_id)[which_camera], "_camera")

  names(df_id)[names(df_id) == "landmark_id"] <- "landmark_camera_id"
  names(df_id)[names(df_id) == "landmark_error"] <- "landmark_camera_error"
  #--------------------------------------------------------------------------
  # 3. Inject lure landmark
  #--------------------------------------------------------------------------
  # Remove `_appat` from colnames
  df_appat <- df_id
  names(df_appat)[grep("_appat", names(df_appat))] <- sub('_appat', "\\1", names(df_appat)[grep("_appat", names(df_appat))], perl = TRUE)
  # Hide `device_id` column name
  names(df_appat)[names(df_appat) == "device_id"] <- "device_id_camera"
  # Prep requests
  requests <- df_appat |>
    coleo_injection_prep(db_table = "landmarks", schema = schema)
  # Injection
  response <- coleo_injection_execute(requests) # Real thing
  # Output
  injection_reponse_message("landmark_appat", response)
  # Finalizing landmarks table injection
  df_id <- response |>
    coleo_injection_final()
  
  # Inject camera landmark into observation_landmarks_lookup table
  df_id <- coleo_inject_table(df_id, "observations_landmarks_lookup", schema = schema)

  # Bring back `device_id` column name
  names(df_id)[names(df_id) == "device_id_camera"] <- "device_id"

  # Rename appat columns
  which_appat <- grepl("landmarks", names(df_id)) & !grepl("_camera", names(df_id))
  names(df_id)[which_appat] <- paste0(names(df_id)[which_appat], "_appat")

  names(df_id)[names(df_id) == "landmark_id"] <- "landmark_lure_id"
  names(df_id)[names(df_id) == "landmark_error"] <- "landmark_lure_error"

  #--------------------------------------------------------------------------
  # 4. Order columns
  #--------------------------------------------------------------------------
  df_id <- df_id[,order(colnames(df_id))] |>
      dplyr::relocate(dplyr::ends_with("_error")) |>
      dplyr::relocate(dplyr::ends_with("_id"))

  return(df_id)
}


#' Injection des colonnes taxa_names dans la table ref_species de coleo.
#'
#' Accepte un vecteur de noms de taxons et performe l'injection de la table
#' ref_species.
#'
#' Cette fonction est silencieuse et ne retourne aucun message. Tous les taxons
#' seront injectés dans la table ref_species, mais ceux déjà présent
#' retourneront une erreur.
#'
#' Cette fonction est utilisée par \code{\link[rcoleo]{coleo_inject}} et
#' \code{\link[rcoleo]{coleo_inject_mam_landmarks}}.
#'
#' @param taxa_col Un vecteur de taxa_names à injecter.
#' @param schema Schéma sur lequel faire la requête. Par défaut, le schéma
#' public est utilisé.
#'
coleo_inject_ref_species <- function(taxa_col, schema = 'public') {
  # Remove duplicated names
  taxa_col <- as.data.frame(taxa_col)
  taxa_col <- taxa_col[!duplicated(taxa_col),] |>
    as.data.frame()
  colnames(taxa_col) <- "ref_species_name"

  if (length(taxa_col) > 0) {
    taxa_col |>
      coleo_injection_prep(db_table = "ref_species", schema = schema) |>
      coleo_injection_execute()
  }
}


#' Injection des campagnes végétation_transect dans la table campaigns de coleo.
#'
#' Les campagnes de transects de végétation nécessitent tout d'abord de vérifier si les campagnes existent.
#' 
#' Accepte un data.frame validé par \code{\link[rcoleo]{coleo_validate}} et performe
#' l'injection de la table campaigns.
#'
#' La fonction est utilisée par \code{\link[rcoleo]{coleo_inject}}.
#'
#' @param df_id Un data.frame contenant les données de végétation_transect à injecter.
#' @param schema Schéma sur lequel faire la requête.
#'
#' @return Une data.frame avec une colonne campaign_id et une colonne pour
#' les campaign_error.
#'
coleo_inject_vegetation_transect_campaigns <- function(df_id, schema = 'public') {
  # La distinction entre les nouvelles campagnes et celles déjà existantes est effectuée en utilisant les colonnes site_id, campaigns_opened_at et sites_site_code. On suppose que les techniciens sont les mêmes, car cette colonne n'est pas utilisée dans les critères d'unicité. Cela vise à limiter la duplication des campagnes dans coleo en cas de changements dans le formatage des noms.
  # L'injection se fait en trois étapes :
  #   1. Check if campaigns already exists in coleo
  #   2. Inject campaigns that are not in coleo
  #   3. Inject other tables

  #-------------------------------------------------------------------------
  # 1. Check if campaigns already exists in coleo
  #-------------------------------------------------------------------------
  # Get veg_campaigns in coleo
  veg_campaigns <- coleo_request_general(endpoint = "campaigns", response = TRUE, schema = schema, "type" = "eq.végétation_transect")

  # If no campaigns in coleo, inject all campaigns
  if (nrow(veg_campaigns) == 0) return(coleo_inject_table(df_id, "campaigns", schema = schema))

  # If campaigns in coleo, check if any in df_id
  veg_campaigns <- subset(veg_campaigns, select = c(id, site_id, opened_at))

  # Add site_code to veg_campaigns
  site_code <- coleo_request_general(endpoint = "sites", response = TRUE, schema = schema, "id" = paste0("in.(",paste(veg_campaigns$site_id, collapse = ","), ")")) |>
    dplyr::select(id, site_code)

  # Join veg_campaigns and site_code
  veg_campaigns <- veg_campaigns |>
    dplyr::left_join(site_code, by = c("site_id" = "id")) |>
    dplyr::rename(campaign_id = id, campaigns_opened_at = opened_at, sites_site_code = site_code)

  # Check if existing veg_campaigns in df
  df_c_id <- df_id |>
    dplyr::left_join(veg_campaigns, by = c("sites_site_code", "campaigns_opened_at")) |>
    as.data.frame()

  #-------------------------------------------------------------------------
  # 2. Isolate campaigns to be injected
  #-------------------------------------------------------------------------
  ## Isolate campaigns that are already in coleo
  df_camp <- df_c_id[!is.na(df_c_id$campaign_id),] |>
    dplyr::mutate(campaign_error = NA) |>
    dplyr::relocate(campaign_id, site_id, campaign_error)
  
  ## Isolate campaigns that are not yet in coleo
  df <- df_c_id[is.na(df_c_id$campaign_id),] |> subset(select=-campaign_id) |> subset(select=-site_id)

  #-------------------------------------------------------------------------
  # 3. Inject campaigns that are not in coleo
  #-------------------------------------------------------------------------
  if (any(is.na(df$campaign_id))) df_id <- coleo_inject_table(df, "campaigns", schema = schema)

  #-------------------------------------------------------------------------
  # 4. Bind all campaigns
  #-------------------------------------------------------------------------
  # Join df_id (injected campaigns) to df_camp (existing campaigns)
  if (nrow(df) > 0) {
    df_id <- dplyr::bind_rows(df_id, df_camp)
  } else {
    df_id <- df_camp
  }

  # Return the results
  return(df_id)
}