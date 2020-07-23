test_that("doTransform works", {
  d <- data.frame(domain = "Domain", test = rep(paste("Test", 1:3), each = 8),
                  visit = rep(paste("Visit", 1:4), each = 2),
                  arm = rep(c("A", "B"), 4))
  d <- bind_rows(d, d) %>%
    mutate(subject = paste0("subject", 1:48),
           value = rnorm(48))

  expect_error(doTransform(d, domain = "domain", test = "test"), label = "doTransform requires geometric argument")
  expect_error(doTransform(d, domain = "domain", test = "test", geometric = FALSE), label = "doTransform requires zeros argument")

  tr <- doTransform(d, domain = "domain", test = "test", geometric = FALSE, zeros = NULL)

  expect_true(all(tr$value == tr$..tvalue..), label = "doTransform returns untransformed data when geometric is false")

  # All values > 0
  d$value <- exp(d$value)
  tr <- doTransform(d, domain = "domain", test = "test", geometric = TRUE, zeros = NULL)
  expect_true(all(tr$value == tr$..tvalue..),
              label = "doTransform returns log(value) if geometric is true and zeros is null, all > 0")

  # Include a zero: omit
  d$value[1] <- 0
  tr <- doTransform(d, domain = "domain", test = "test", geometric = TRUE, zeros = "omit")
  expect_true(all(tr$..tvalue.. == log(d$value)),
              label = "doTransform returns logs if zeros = omit")

  # Include a zero: add1
  d$value[1] <- 0
  tr <- doTransform(d, domain = "domain", test = "test", geometric = TRUE, zeros = "add1")

  s <- split(tr, tr$test)

  expect_true(all(s[[1]]$..tvalue.. == log1p(s[[1]]$value)),
              label = "doTransform returns log1p if zeros = add1, split by test")
  expect_true(all(s[[2]]$..tvalue.. == log(s[[2]]$value)),
              label = "doTransform returns log if zeros = add1, no 0s for the test")
  expect_true(all(s[[3]]$..tvalue.. == log(s[[3]]$value)),
              label = "doTransform returns log if zeros = add1, no 0s for the test")
})
