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


    #--------------------------------------------------------------------------
    # 2. Format dataset
    #--------------------------------------------------------------------------
    dataFile <- coleo_format(dataFile)


    return(dataFile)
}