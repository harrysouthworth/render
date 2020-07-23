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
    }
  }

  invisible()
}

doTransform <- function(data, domain, test, geometric, zeros){
  if (is.null(zeros)){
    zeros <- "bleargh"
  }

  getTranny <- function(x){
    if (geometric){
      if (zeros == "add1"){
        log1p(x)
      } else if (zeros %in% c("bleargh", "omit")){
        log(x)
      } else {
        stop("you shouldn't be here!")
      }
    } else {
      x
    }
  }

  group_by(data, domain, test) %>%
    mutate(tvalue = getTranny(value)) %>%
    as.data.frame()
}

hilo <- function(x, which, g, z0, alpha = .05){
  minx <- min(x)
  x <- x[!is.na(x) & x > -Inf]
  z <- qnorm(1 - alpha / 2)

  se <- z * sqrt(stats::var(x)/length(x))

  if (which == "mean"){
    res <- mean(x)
  } else if (which == "lo"){
    res <- mean(x) - se
  } else {
    res <- mean(x) + se
  }

  if (g){
    if (z0 == "add1" & minx == -Inf){
      exp(res) - 1
    } else if (z0 %in% c("add1", "omit")) {
      exp(res)
    }
  } else{
    res
  }
}
