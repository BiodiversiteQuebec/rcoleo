context("gen_gen")

test_that("API returns sensible error message when not authorised", {
  ## Once the api key is set up, you'll need to make it *not* available here, something like
  withr::with_envvar(c(RCOLEO_TOKEN = NA_character_), {
    expect_error(
      get_gen("/cells"),
      "Aucune autorisation d\u00e9tect\u00e9e")
  })
})
