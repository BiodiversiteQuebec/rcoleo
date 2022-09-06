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
    testthat::expect_silent(data <- coleo_read_csv(fileName))

    # Test that coleo_read_csv reads the csv file and returns a data.frame
    testthat::expect_s3_class(data, "data.frame")

    # The formating of columns is assumed by `coleo_format()`

    # Remove dummy csv file
    remove(fileName)
})
