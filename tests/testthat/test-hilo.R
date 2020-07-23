test_that("hilo works", {
  x <- rnorm(100)

  h <- hilo(x, which = "mean", g = FALSE, z = NULL)
  expect_true(h == mean(x), label = "hilo produces mean")

  z <- qnorm(.975)
  ms <- mean_se(x, mult = z)

  h <- hilo(x, which = "lo", g = FALSE, z = NULL)
  expect_equal(unname(ms[1, "ymin"]), h, label = "hilo produces lower 95% limit")

  h <- hilo(x, which = "hi", g = FALSE, z = NULL)
  expect_equal(unname(ms[1, "ymax"]), h, label = "hilo produces lower 95% limit")

  # add1
  x <- exp(x)
  h <- hilo(x, which = "mean", g = TRUE, z0 = "add1")
  expect_equal(h, exp(mean(x)), label = "hilo produces exp(mean(x)), no 0s")


  ms <- mean_se(x, mult = z)


  h <- hilo(x, which = "lo", g = TRUE, z0 = "add1")
  expect_equal(h, exp(ms[1, "ymin"]), label = "hilo produces exp(ymin(x)), no 0s")

  h <- hilo(x, which = "hi", g = TRUE, z0 = "add1")
  expect_equal(h, exp(ms[1, "ymax"]), label = "hilo produces exp(ymax(x)), no 0s")

  # omit
  h <- hilo(x, which = "mean", g = TRUE, z0 = "omit")
  expect_equal(h, exp(mean(x)), label = "hilo produces exp(mean(x)), no 0s")

  h <- hilo(x, which = "lo", g = TRUE, z0 = "omit")
  expect_equal(h, exp(ms[1, "ymin"]), label = "hilo produces exp(ymin(x)), no 0s")

  h <- hilo(x, which = "hi", g = TRUE, z0 = "omit")
  expect_equal(h, exp(ms[1, "ymax"]), label = "hilo produces exp(ymax(x)), no 0s")


  # with 0s
  x[1] <- -Inf
  ms <- mean_se(x[-1], mult = z)

  h <- hilo(x, which = "mean", g = TRUE, z0 = "omit")
  expect_equal(h, exp(mean(x[-1])), label = "hilo produces exp(mean(x)), 0s, omit")

  h <- hilo(x, which = "lo", g = TRUE, z0 = "omit")
  expect_equal(h, exp(ms[1, "ymin"]), label = "hilo produces exp(mean(x)), 0s, omit")

  h <- hilo(x, which = "hi", g = TRUE, z0 = "omit")
  expect_equal(h, exp(ms[1, "ymax"]), label = "hilo produces exp(ymax(x)), 0s, omit")


  x <- rnorm(100, 2)
  x[x < 0] <- 0
  x <- log1p(x)

  h <- hilo(x, which = "mean", g = TRUE, z0 = "add1")
  expect_equal(h, exp(mean(x)) - 1)


})
