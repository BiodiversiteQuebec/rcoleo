# Test that `coleo_format()` returns a data.frame with the columns in the right class
test_that("coleo_format returns a data.frame with the columns in the right class", {
    # Save dummy csv file
    dataFrame <- data.frame(
        campaigns_type = "mammifères",
        campaigns_technicians = "Alfred Bilot, Amande Laurier",
        efforts_photo_count = "1"
    )

    # Run coleo_format
    dataFormated <- coleo_format(dataFrame)

    # Test that coleo_format returns a data.frame
    testthat::expect_s3_class(data, "data.frame")

    # Test that coleo_format reads formats the columns
    ## - Character columns remains as characters
    testthat::expect_type(data$campaigns_type, "character")
    ## - Numeric columns are converted to numeric
    testthat::expect_type(data$efforts_photo_count, "integer")
    ## - List columns are converted to list
    testthat::expect_type(data$campaigns_technicians, "list")
})