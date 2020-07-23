test_that("checkGeometric works", {
  x <- -1:10

  expect_null(checkGeometric(x, geom = FALSE), label = "geometric not requested returns null")

  expect_error(checkGeometric(x, geom = TRUE, label = "geometric but negative values is error"))

  x <- 1:10
  expect_null(checkGeometric(x, geom = TRUE), label = "geometric and all positive returns null")

  x <- 0:10
  expect_error(checkGeometric(x, geom = TRUE), label = "geometric with 0s requires z")
  expect_error(checkGeometric(x, geom = TRUE, z = "bleargh"), label = "geometric with 0s requires z to be 'add1' or 'omit'")

  expect_null(checkGeometric(x, geom = TRUE, z = "omit"), label = "geometric with z = omit returns null")
  expect_null(checkGeometric(x, geom = TRUE, z = "add1"), label = "geometric with z = omit returns null")
})
