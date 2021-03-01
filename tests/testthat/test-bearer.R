test_that("check authenification correctly", {
  withr::with_envvar(c(RCOLEO_TOKEN = NA_character_), {
    expect_error(
      bearer(), "Aucune autorisation d\u00e9tect\u00e9e")
  })

  withr::with_envvar(c(RCOLEO_TOKEN = "Hi hello"), {
    expect_equal(
      bearer(), "Hi hello")
  })

  # how to check for the file? make it then destroy it??
})
