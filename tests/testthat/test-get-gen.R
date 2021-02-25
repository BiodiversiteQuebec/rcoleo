context("gen_gen")

test_that("API returns sensible error message when not authorised", {
  ## Once the api key is set up, you'll need to make it *not* available here, something like
  withr::with_envvar(c(MY_KEY = NA_character_), {
    expect_error(
      get_gen("/cells"),
      "Aucune autorisation détectée")
  })
})
