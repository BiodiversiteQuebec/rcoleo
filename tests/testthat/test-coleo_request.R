

without_internet({
  test_that("coleo_request makes a good request", {

    expect_GET(coleo_request_general(cell_code = "foo", endpoint = "cells"),
               url = "https://coleo.biodiversite-quebec.ca/api/v1/cells?cell_code=foo")

  })

})


with_mock_dir("coleo_request", {
  test_that("no answer for a nonsense code", {
    nonsense_request <- coleo_request_general(cell_code = "foo", endpoint = "cells")

    expect_equal(httr2::resp_body_json(nonsense_request), list())
  })
})

# query for a real site

with_mock_dir("coleo_request_real", {
  test_that("returns answer for a real code",{
    real_site <- coleo_request_general(cell_code = "139_87", endpoint = "cells")

    answer <- httr2::resp_body_json(real_site)[[1]]

    expect_equal(length(answer), 7)

  })
})


with_mock_dir("coleo_request_real", {
    real_cell <- coleo_request_general(cell_code = "139_87", endpoint = "cells")

    resp_body <- httr2::resp_body_json(real_cell)[[1]]

    expect_equal(length(resp_body), 7)

    # can pluck one id successfully from site request
    expect_equal(coleo_pluck_one_id(real_cell), 161)

})

test_that("errors for bad endpoint", {
  expect_error(coleo_request_general(endpoint = "not_an_endpoint"))
})

with_mock_dir("site id download and extraction", {

  real_site <- coleo_request_general(site_code = "139_87_F01", endpoint = "sites")

  test_that("request_general works for a valid site", {


    resp_body <- httr2::resp_body_json(real_site)[[1]]

    # should be 14 columns of info for this site
    expect_equal(length(names(resp_body)), 14)

  })

  test_that("can coleo_pluck_id correctly", {
    expect_equal(coleo_pluck_one_id(real_site), 94)
  })

  })
