#' Reads a data file and format columns to coleo requirements
#'
#' @param filePath Local path to a data file.
#'
#' @return data.frame with formated columns
#'
#' @export
#'
coleo_read <- function(filePath) {
  # Catch the file extension
  ex <- strsplit(basename(filePath), split="\\.")[[1]][2]

  # Read the data
  if (ex == "csv") dataFile <- coleo_read_csv(filePath)
  else if (ex %in% c("xls", "xlsx")) dataFile <- coleo_read_template(filePath)
  
  # Test that the extension is acceptable
  if (!ex %in% c("csv", "xls", "xlsx")) warning("Seuls les documents csv et les templates coleo sont pris en charge au moment. Veuillez soumettre les données dans un document csv ou dans le template coleo.")
  else return(dataFile)
}


#' Read csv data and format columns to coleo requirements
#'
#' @param fileName Local path to csv file.
#'
#' @return a data.frame with formated columns
#'
#' @export
#'
coleo_read_csv <- function(fileName) {
    #--------------------------------------------------------------------------
    # 1. Get data
    #--------------------------------------------------------------------------
    dataFile <- try(read.csv(fileName, colClasses = "character", na.strings = "", stringsAsFactors = FALSE), silent=TRUE)

    # Some csv files may have ";" separators instead of "," - thanks to french Excel...
    readCsv2 <- function (fileName) read.csv2(fileName, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
    if (inherits(dataFile, "try-error")) {
      dataFile <- readCsv2(fileName)
    } else if (ncol(dataFile) == 1) dataFile <- readCsv2(fileName)
    # Remove added column
    if ('...1' %in% colnames(dataFile)) dataFile <- dataFile[,-1]
    if ('X' %in% colnames(dataFile)) dataFile <- subset(dataFile, select = -X)


    #--------------------------------------------------------------------------
    # 2. Format dataset
    #--------------------------------------------------------------------------
    dataFile <- coleo_format(dataFile)


    return(dataFile)
}


#' Read data directly from a coleo template and format columns to coleo requirements
#'
#' @param templatePath Local path to template file.
#'
#' @return a data.frame with formated columns
#'
#' @export
#'
coleo_read_template <- function(templatePath) {
    #--------------------------------------------------------------------------
    # 1. Get data
    #--------------------------------------------------------------------------
    dataFile <- readxl::read_excel(templatePath, sheet = "6. Template de téléversement", col_types = "text") |>
      as.data.frame()
    # Remove added column
    if ('...1' %in% colnames(dataFile)) dataFile <- dataFile[,-1]
    if ('X' %in% colnames(dataFile)) dataFile <- dataFile[,-"X"]


    #--------------------------------------------------------------------------
    # 2. Format dataset
    #--------------------------------------------------------------------------
    dataFile <- coleo_format(dataFile)


    return(dataFile)
}