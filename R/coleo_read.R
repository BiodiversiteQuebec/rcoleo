#' Importe les données à injecter depuis un template coleo ou un document csv
#' et formate les colonnes pour l'injection dans la base de données coleo.
#' 
#' Importe les données et formate la classe des colonnes pour injection dans la base de données coleo.
#'
#' @param filePath Chemin local vers le document de données.
#'
#' @return Un data.frame avec les colonnes formatées pour l'injection à coleo.
#'
#' @export
#'
coleo_read <- function(filePath) {
  #-----------------------------------------------------------------------------
  # Load the data
  #-----------------------------------------------------------------------------
  # Catch the file extension
  ex <- strsplit(basename(filePath), split="\\.")[[1]][2]

  # Read the data
  if (ex == "csv") dataFile <- coleo_read_csv(filePath)
  else if (ex %in% c("xls", "xlsx")) dataFile <- coleo_read_template(filePath)
  else if (ex %in% "shp") {
    dataFile <- coleo_read_shape(filePath)

    return(dataFile)
  }

  #-----------------------------------------------------------------------------
  # Format the data
  # - Except for shapefiles
  #-----------------------------------------------------------------------------
  # Test that the extension is acceptable
  if (!ex %in% c("csv", "xls", "xlsx")) warning("Seuls les documents csv, shp et les templates coleo sont pris en charge au moment. Veuillez soumettre les données dans un document csv ou dans le template coleo.")
  else if ("campaigns_type" %in% names(dataFile)) {
    # Format dataset
    dataFile <- coleo_format(dataFile)

    return(dataFile)
  } else if ("sites_type" %in% colnames(dataFile)) {
    # Format sites dataset
    dataFile$sites_lat <- as.numeric(dataFile$sites_lat)
    dataFile$sites_lon <- as.numeric(dataFile$sites_lon)

    return(dataFile)
  }

  return(dataFile)
}


#' Importe les données à injecter depuis un document csv.
#'
#' @param fileName Chemin local vers le document csv.
#'
#' @return Un data.frame avec les colonnes formatées pour l'injection à coleo.
#'
#' @export
#'
coleo_read_csv <- function(fileName) {
    # Get data
    dataFile <- try(read.csv(fileName, colClasses = "character", na.strings = "", stringsAsFactors = FALSE), silent=TRUE)

    # Some csv files may have ";" separators instead of "," - thanks to french Excel...
    readCsv2 <- function (fileName) read.csv2(fileName, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
    if (inherits(dataFile, "try-error")) {
      dataFile <- readCsv2(fileName)
    } else if (ncol(dataFile) == 1) dataFile <- readCsv2(fileName)
    # Remove added column
    if ('...1' %in% colnames(dataFile)) dataFile <- dataFile[,-1]
    if ('X' %in% colnames(dataFile)) dataFile <- subset(dataFile, select = -X)

    return(dataFile)
}


#' Importe les données à injecter depuis un template coleo
#'
#' @param templatePath Chemin local vers le document csv.
#'
#' @return Un data.frame avec les colonnes formatées pour l'injection à coleo.
#'
#' @export
#'
coleo_read_template <- function(templatePath) {
    # 1. Get data
    dataFile <- readxl::read_excel(templatePath, sheet = "6. Template de téléversement", col_types = "text") |>
      as.data.frame()
    # Remove added column
    if ('...1' %in% colnames(dataFile)) dataFile <- dataFile[,-1]
    if ('X' %in% colnames(dataFile)) dataFile <- dataFile[,-"X"]

    return(dataFile)
}


#' Importe les données de cellules à injecter depuis un shapefile.
#'
#' @param filePath Chemin local vers le document csv.
#'
#' @return Un data.frame avec les colonnes formatées pour l'injection à coleo.
#'
#' @export
#'
coleo_read_shape <- function(filePath) {
    #--------------------------------------------------------------------------
    # 1. Get data
    #--------------------------------------------------------------------------
    cell_data <- sf::read_sf(filePath)

    #--------------------------------------------------------------------------
    # 2. Rename geometry column
    #--------------------------------------------------------------------------
    sf::st_geometry(cell_data) <- "geom"

    return(cell_data)
}