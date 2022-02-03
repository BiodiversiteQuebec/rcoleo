

without_internet({
  test_that("coleo_request makes a good request", {

    expect_GET(coleo_request_general(cell_code = "foo", table = "cells"),
               url = "https://coleo.biodiversite-quebec.ca/api/v1/cells?cell_code=foo")

  })

})


with_mock_dir("coleo_request", {
  test_that("no answer for a nonsense code", {
    nonsense_request <- coleo_request_general(cell_code = "foo", table = "cells")

    expect_equal(httr2::resp_body_json(nonsense_request), list())
  })
})

# query for a real site

with_mock_dir("coleo_request_real", {
  test_that("returns answer for a real code",{
    real_site <- coleo_request_general(cell_code = "139_87", table = "cells")

    answer <- httr2::resp_body_json(real_site)[[1]]

    expect_equal(length(answer), 7)

  })
})


with_mock_dir("coleo_request_real", {
    real_site <- coleo_request_general(cell_code = "139_87", table = "cells")

    resp_body <- httr2::resp_body_json(real_site)[[1]]

    expect_equal(length(resp_body), 7)
})
