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
    campaign_type <- unique(dataFile$campaigns_type)
    data_cols <- rcoleo::coleo_return_cols(campaign_type)
    for(col in seq_along(names(dataFile))) {
      true_col_nm <- names(dataFile)[col]

        if(!true_col_nm %in% data_cols$noms_de_colonnes) {
          # Extra columns must remain as characters
          if (grepl("_extra", true_col_nm)) next
          ## Some columns may have modified names
          ## eg. lures_lure_1
          mod_col_nm <- sapply(data_cols$noms_de_colonnes,
            function(x) grepl(x, true_col_nm)
          )
          if (any(mod_col_nm)) {
            true_col_nm <- names(which(mod_col_nm))
            ## Select the longuest name - In case there is multiple matching
            true_col_nm <- true_col_nm[which.max(nchar(true_col_nm))]
          } else next
        }

        colClass <- data_cols[data_cols$noms_de_colonnes == true_col_nm, "classe"]

        if(colClass == "list") {
            dataFile[,col] <- list(stringr::str_split(dataFile[,col], ", |,"))
        } else {
            asClass <- paste0('as.', colClass)
            dataFile[,col] <- .Primitive(asClass)(dataFile[,col], drop = FALSE)
        }
    }

    return(dataFile)
}