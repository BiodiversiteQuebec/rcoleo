context("test injection prep: COLEO_comp")

test_that("COLEO_comp gives useful errors or messages", {
  badlist <- list(a = 1, b = 2)
  xs <- letters[1:5]

  expect_error(COLEO_comp(badlist$c, letters))
  expect_message(COLEO_comp(xs, letters), "Vous pouvez passer")
  expect_message(COLEO_comp(letters, xs), "Coleo ne contient pas")
})

test_that("COLEO_comp returns the right thing", {
  xs <- letters[1:5]

  expect_equal(COLEO_comp(xs, LETTERS), xs)
  expect_message(COLEO_comp(xs, LETTERS), "a, b, c, d, e")

})
