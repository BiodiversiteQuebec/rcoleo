# Test that coleo_read_csv returns a data.frame from a csv file local path
test_that("coleo_read_csv returns a data.frame from a csv file local path", {
    # Save dummy csv file
    fileName <- paste0(tempdir(), "test.csv")
    write.csv(
        data.frame(campaigns_type = "acoustique",
            campaigns_technicians = "Alfred Bilot, Amande Laurier",
            efforts_photo_count = "1"
        ),
        fileName)

    # Test that the file is read without error
    testthat::expect_silent(data <- coleo_read_csv(fileName))

    # Test that coleo_read_csv reads the csv file and returns a data.frame
    testthat::expect_s3_class(data, "data.frame")

    # Test that coleo_read_csv reads the csv file and formats the columns
    ## - Character columns remains as characters
    testthat::expect_type(data$campaigns_type, "character")
    ## - Numeric columns are converted to numeric
    testthat::expect_type(data$efforts_photo_count, "integer")
    ## - List columns are converted to list
    testthat::expect_type(data$campaigns_technicians, "list")

    # Remove dummy csv file
    remove(fileName)
})
