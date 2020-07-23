test_that("checkGeometric works", {
  x <- -1:10

  expect_equal(checkGeometric(x, geom = FALSE), "nothing", label = "geometric not requested returns 'nothing'")

  expect_error(checkGeometric(x, geom = TRUE, label = "geometric but negative values is error"))

  x <- 1:10
  expect_equal(checkGeometric(x, geom = TRUE, z = "omit"), "omit", label = "geometric and all positive returns 'omit'")

  x <- 0:10
  expect_error(checkGeometric(x, geom = TRUE), label = "geometric with 0s requires z")
  expect_error(checkGeometric(x, geom = TRUE, z = "bleargh"), label = "geometric with 0s requires z to be 'add1' or 'omit'")

  expect_equal(checkGeometric(x, geom = TRUE, z = "omit"), "omit", label = "geometric with z = omit returns omit")
  expect_equal(checkGeometric(x, geom = TRUE, z = "add1"), "add1", label = "geometric with z = add1 returns add1")
})
