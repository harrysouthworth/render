test_that("correct t-stats are returned", {
  d <- data.frame(domain = "Domain", test = rep(paste("Test", 1:3), each = 8),
                  visit = rep(paste("Visit", 1:4), each = 2),
                  arm = rep(c("A", "B"), 4))
  d <- bind_rows(d, d) %>%
    mutate(subject = paste0("subject", 1:48),
           value = rnorm(48))

  fs <- fullSummary(d, ci = TRUE, pvalue = TRUE, approx = "t")

  s <- split(d, list(d$domain, d$visit, d$arm, d$test))
  cis <- sapply(s, function(X){
    confint(lm(value ~ 1, data = X))
  }) %>% t()

  expect_equivalent(fs$Lo, cis[, 1], tol = 1e-5,
                    label = "Lower t-limits are correct")
  expect_equivalent(fs$Hi, cis[, 2], tol = 1e-5,
                    label = "Upper t-limits are correct")

  ps <- sapply(s, function(X){
    coef(summary(lm(value ~ 1, data = X)))[1, 4]
  })

  expect_equivalent(fs$`p-value`, ps, tol = 1e-5,
                    label = "p-values from  t-statistics are correct")

})

test_that("geometric mean works", {
  d <- data.frame(domain = "Domain", test = rep(paste("Test", 1:3), each = 8),
                  visit = rep(paste("Visit", 1:4), each = 2),
                  arm = rep(c("A", "B"), 4))
  d <- bind_rows(d, d) %>%
    mutate(subject = paste0("subject", 1:48),
           value = rnorm(48))


  fs <- fullSummary(d)

  sdat <- split(d, list(d$domain, d$visit, d$arm, d$test))
  ssd <- sapply(sdat, function(X) sd(X$value))
  sq1 <- sapply(sdat, function(X) quantile(X$value, prob = .25))

  expect_true(all(fs$SD == ssd), label = "simple sd is fine")
  expect_true(all(fs$Q1 == sq1), label = "simple sd is fine")

  ## geometric mean with negatives
  expect_error(fullSummary(d, geometric = TRUE, ci = TRUE), label = "error for gmean with negatives")

  ## Geometric mean with no 0s
  d$value <- rnorm(48, 10)
  fs <- fullSummary(d, geometric = TRUE, ci = TRUE, approx = "z")

  sdat <- split(d, list(d$domain, d$visit, d$arm, d$test))
  sgm <- sapply(sdat, function(X) exp(mean(log(X$value))))
  slo <- sapply(sdat, function(X) exp(mean(log(X$value) - qnorm(.975) * sd(log(X$value)) / sqrt(nrow(X)))))
  shi <- sapply(sdat, function(X) exp(mean(log(X$value) + qnorm(.975) * sd(log(X$value)) / sqrt(nrow(X)))))

  expect_true(all(fs$Gmean == sgm), label = "no negatives, geometric mean ok")
  expect_equivalent(fs$Lo, slo, tol = 1e-5, label = "no negatives, geometric lo ok")
  expect_equivalent(fs$Hi, shi, tol = 1e-5, label = "no negatives, geometric hi ok")

  ## Geometric mean with 0s omitted
  d$value <- rnorm(48, 1)
  d$value <- pmax(d$value, 0)

  sdat <- split(d, list(d$domain, d$visit, d$arm, d$test))

  expect_error(fullSummary(d, geometric = TRUE, ci = TRUE),
               label = "fail when 0s but no zeros argument")

  fs <- fullSummary(d, geometric = TRUE, ci = TRUE, zeros = "omit", approx = "z")

  sgm <- sapply(sdat, function(X) exp(mean(log(X$value[X$value > 0]))))
  slo <- sapply(sdat, function(X) exp(mean(log(X$value[X$value > 0]) - qnorm(.975) * sd(log(X$value[X$value > 0])) / sqrt(nrow(X)))))
  shi <- sapply(sdat, function(X) exp(mean(log(X$value[X$value > 0]) + qnorm(.975) * sd(log(X$value[X$value > 0])) / sqrt(nrow(X)))))

  expect_equivalent(fs$Gmean, sgm, label = "omit 0s geometric mean ok")
  expect_equivalent(fs$Lo, slo, tol = 1e-5, label = "omit 0s geometric lo ok")
  expect_equivalent(fs$Hi, shi, tol = 1e-5, label = "omit 0s geometric hi ok")

  ## Geometric mean with log1p
  fs <- fullSummary(d, geometric = TRUE, ci = TRUE, zeros = "add1", approx = "z")
  wh <- fs$Gmean == sgm
  wh[is.na(wh)] <- FALSE
  expect_true(!any(wh), label = "using log1p produces different results to using omit")

  sgm <- sapply(sdat, function(X) expm1(mean(log1p(X$value))))
  slo <- sapply(sdat, function(X) expm1(mean(log1p(X$value) - qnorm(.975) * sd(log1p(X$value)) / sqrt(nrow(X)))))
  shi <- sapply(sdat, function(X) expm1(mean(log1p(X$value) + qnorm(.975) * sd(log1p(X$value)) / sqrt(nrow(X)))))

  expect_equivalent(fs$Gmean, sgm, label = "log1p geometric mean ok")
  expect_equivalent(fs$Lo, slo, tol = 1e-5, label = "log1p geometric lo ok")
  expect_equivalent(fs$Hi, shi, tol = 1e-5, label = "log1p geometric hi ok")
})
