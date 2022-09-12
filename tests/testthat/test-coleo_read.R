# Test that coleo_read_data returns a data.frame from a file local path
test_that("coleo_read_data returns a data.frame from a file local path", {
    # Save dummy csv file
    fileNameCsv <- paste0(tempdir(), "test.csv")
    write.csv(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileNameCsv)
    # Save dummy xlsx file
    fileNameXl <- paste0(tempdir(), "test.xlsx")
    xlsx::write.xlsx(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileNameXl,
        sheetName = "6. Template de téléversement")

    # Test that the file is read without error
    testthat::expect_silent(coleo_read_data(fileNameCsv), NA)
    testthat::expect_error(coleo_read_data(fileNameXl), NA)

    # Test that coleo_read_data reads the csv and xlsx files and returns a data.frame
    testthat::expect_s3_class(dataCsv, "data.frame")
    testthat::expect_s3_class(dataXl, "data.frame")

    # Test that coleo_read_data returns the right columns
    colNames <- c("campaigns_type", "campaigns_technicians", "efforts_photo_count")
    testthat::expect_identical(names(dataCsv), colNames)
    testthat::expect_s3_class(names(dataXl), colNames)

    # Remove dummy csv file
    remove(list = c("fileNameCsv", "fileNameXl"))
})


# Test that coleo_read_csv returns a data.frame from a csv file local path
test_that("coleo_read_csv returns a data.frame from a csv file local path", {
    # Save dummy csv file
    fileName <- paste0(tempdir(), "test.csv")
    write.csv(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileName)

    # Test that the file is read without error
    testthat::expect_error(data <- coleo_read_csv(fileName), NA)

    # Test that coleo_read_csv reads the csv file and returns a data.frame
    testthat::expect_s3_class(data, "data.frame")

    # The formating of columns is assumed by `coleo_format()`

    # Remove dummy csv file
    remove(fileName)
})


# Test that coleo_read_template returns a data.frame from a template file local path
test_that("coleo_read_template returns a data.frame from a file local path", {
    # Save dummy xlsx file
    fileName <- paste0(tempdir(), "test.xlsx")
    xlsx::write.xlsx(
        data.frame(campaigns_type = "mammifères",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileName,
        sheetName = "6. Template de téléversement")

    # Test that the file is read without error
    testthat::expect_silent(coleo_read_template(fileName), NA)

    # Test that coleo_read_csv reads the csv file and returns a data.frame
    testthat::expect_s3_class(data, "data.frame")

    # The formating of columns is assumed by `coleo_format()`

    # Remove dummy csv file
    remove(fileName)
})
