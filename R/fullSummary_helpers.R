checkGeometric <- function(x, geom, z){
  if (geom){
    if (min(x, na.rm = TRUE) < 0){
      stop("geometric mean requested, but negative values in data")
    } else if (min(x, na.rm = TRUE) == 0){
      if (is.null(z)){
        stop("geometric is TRUE but there are 0s in the data. Use the zeros argument to tell me to 'add1' or 'omit'")
      } else if (!(z %in% c("add1", "omit"))){
        stop("zeros should be one of 'add1' or 'omit'")
      }
    } else { # min(x) > 0
    z <- "omit"
    }
  } else {
    z <- "nothing"
  }

  z
}

doTransform <- function(data, domain, test, arm, visit,geometric, zeros){
  getTranny <- function(x){
    if (geometric){
      if (zeros == "add1"){
        if (min(x$value, na.rm = TRUE) == 0){
          x$..tvalue.. <- log1p(x$value)
        } else {
          x$..tvalue.. <- log(x$value)
        }
      } else if (zeros == "omit"){
        x$..tvalue.. <- log(x$value)
      } else {
        stop("you shouldn't be here!")
      }
    } else {
      x$..tvalue.. <- x$value
    }

    x
  }

  data$..index.. <- 1:nrow(data)

  s <- split(data, list(data[, domain], data[, test]))
  s <- lapply(s, getTranny)

  res <- as.data.frame(bind_rows(s))
  res <- res[order(res$..index..), ]
  res$..index.. <- NULL
  res
}

hilo <- function(x, which, g, z0, alpha = .05, approx = "t"){
  x <- x[!is.na(x) & x > -Inf]
  if (approx == "z"){
    z <- qnorm(1 - alpha / 2)
  } else if (approx == "t"){
    if (length(x) > 1){
      z <- qt(1 - alpha / 2, df = length(x) - 1)
    } else {
      z <- 0
    }
  }

  se <- sqrt(stats::var(x)/length(x))
  testStat <- mean(x) / se

  if (approx == "z"){
    p <- 2 * pnorm(abs(testStat), lower.tail = FALSE)
  } else if (approx == "t"){
    if (length(x) > 1){
      p <- 2 * pt(abs(testStat), df = length(x) - 1, lower.tail = FALSE)
    } else {
      p <- 1
    }
  }


  if (which == "mean"){
    res <- mean(x)
  } else if (which == "lo"){
    res <- mean(x) - se * z
  } else if (which == "hi") {
    res <- mean(x) + se * z
  } else if (which == "p-value"){
    res <- p
  }

  if (g & which != "p-value"){
    if (z0 == "add1"){
      expm1(res)
    } else if (z0  == "omit") {
      exp(res)
    }
  } else {
    res
  }
}
