# Test that coleo_read returns error if wrong file extension
test_that("coleo_read returns error if wrong file extension", {
    fileNametxt <- paste0(tempdir(), "/test.txt")
    write.table(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileNametxt)
    testthat::expect_warning(coleo_read(fileNametxt))
})
    
# Test that coleo_read_data returns a data.frame from a file local path
test_that("coleo_read_data returns a data.frame from a file local path", {
    # Save dummy csv file
    fileNameCsv <- paste0(tempdir(), "/test.csv")
    write.csv(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileNameCsv)
    # Save dummy xlsx file
    fileNameXl <- paste0(tempdir(), "/test.xlsx")
    xlsx::write.xlsx(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileNameXl,
        sheetName = "6. Template de téléversement")
    # Save dummy shp file
    fileNameShp <- paste0(tempdir(), "/test.shp")
    sf::st_write(
        data.frame(
            cell_code = "111_111",
            name = "Test",
            geom = sf::st_sfc(sf::st_point(c(-68.784529, 48.112211)))
        ),
        dsn = fileNameShp,
        driver = "ESRI Shapefile",
        delete_dsn = TRUE
    )

    # Test that the file is read without error
    testthat::expect_error(dataCsv <- coleo_read(fileNameCsv), NA)
    testthat::expect_error(dataXl <- coleo_read(fileNameXl), NA)
    testthat::expect_error(dataShp <- coleo_read(fileNameShp), NA)

    # Test that coleo_read_data reads the files and returns a data.frame
    testthat::expect_s3_class(dataCsv, "data.frame")
    testthat::expect_s3_class(dataXl, "data.frame")
    testthat::expect_s3_class(dataShp, c("data.frame", "sf"))

    # Test that coleo_read_data returns the right columns
    colNames <- c("campaigns_type", "campaigns_technicians", "efforts_photo_count")
    testthat::expect_identical(names(dataCsv), colNames)
    testthat::expect_identical(names(dataXl), colNames)
    testthat::expect_identical(names(dataShp), c("cell_code", "name", "geom"))

    # Remove dummy files
    remove(list = c("fileNameCsv", "fileNameXl", "fileNameShp"))
})


# Test that coleo_read_csv returns a data.frame from a csv file local path
test_that("coleo_read_csv returns a data.frame from a csv file local path", {
    # Save dummy csv file
    fileName <- paste0(tempdir(), "/test.csv")
    write.csv(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileName)
    # Save dummy csv2 file
    fileName2 <- paste0(tempdir(), "/test2.csv")
    write.csv2(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileName2)

    # Test that the file is read without error 
    ## (, separator)
    testthat::expect_error(data <- coleo_read_csv(fileName), NA)
    ## (; separator)
    testthat::expect_error(data2 <- coleo_read_csv(fileName2), NA)

    # Test that coleo_read_csv reads the csv file and returns a data.frame
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_s3_class(data2, "data.frame")

    # The formating of columns is assumed by `coleo_format()`

    # Remove dummy csv file
    remove(fileName)
})


# Test that coleo_read_template returns a data.frame from a template file local path
test_that("coleo_read_template returns a data.frame from a file local path", {
    # Save dummy xlsx file
    fileName <- paste0(tempdir(), "/test.xlsx")
    xlsx::write.xlsx(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileName,
        sheetName = "6. Template de téléversement")

    # Test that the file is read without error
    testthat::expect_error(data <- coleo_read_template(fileName), NA)

    # Test that coleo_read_template reads the template file and returns a data.frame
    testthat::expect_s3_class(data, "data.frame")

    # The formating of columns is assumed by `coleo_format()`

    # Remove dummy template file
    remove(fileName)
})



# Test that coleo_read_shape returns a data.frame from a shp file local path
test_that("coleo_read_template returns a data.frame from a file local path", {
    # Save dummy xlsx file
    fileName <- paste0(tempdir(), "/test2.shp")
    sf::st_write(
        data.frame(
            cell_code = "111_111",
            name = "Test",
            geom = sf::st_sfc(sf::st_point(c(-68.784529, 48.112211)))
        ),
        dsn = fileName,
        driver = "ESRI Shapefile",
        delete_dsn = TRUE
    )

    # Test that the file is read without error
    testthat::expect_error(data <- coleo_read_shape(fileName), NA)

    # Test that coleo_read_template reads the template file and returns a data.frame
    testthat::expect_s3_class(data, c("data.frame", "sf"))

    # The formating of columns is assumed by `coleo_format()`

    # Remove dummy template file
    remove(fileName)
})

# Test that coleo_read formats sites data from a csv file local path
test_that("coleo_read formats sites data from a csv file local path", {
    # Save dummy csv file
    fileName <- paste0(tempdir(), "/test.csv")
    write.csv(
        data.frame(sites_lat = "128.5",
            sites_lon = "48.5",
            sites_code = "132_132_F01",
            sites_type = "forestier"
        ),
        fileName)

    # Test that the lat and lon are returned as numeric values
    data <- coleo_read(fileName)
    testthat::expect_type(data$sites_lat, "double")
    testthat::expect_type(data$sites_lon, "double")
})
