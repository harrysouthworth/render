#' Quick check that an analysis dataset has one row per observation ID
#' @param data A data frame that is supposed to have one row per ID (subject).
#' @param id String naming the ID column. Defaults to \code{id = "subject"}.
#' @details The function checks that the data frame has as many rows as unique
#'   values of the ID column, and that it has at least one row.
#' @export
qc <- function (data, id = "subject") {
  s <- data[, id]
  ff1 <- tinytest::expect_equal(nrow(data), length(unique(s)),
                                info = "One row per ID")
  ff2 <- tinytest::expect_true(nrow(data) > 0, info = "Data has rows in it")
  res <- tinytest::expect_true(all(ff1, ff2))

  if (!res){
    stop("Either there is no data are there are duplicate subjects")
  } else {
    invisible(res)
  }
}


#' Shortcut to converting tibbles to data frames
#' @param data A data frame
#' @details I'm sick of having to convert tibbles that hide columns back to
#'   something that doesn't cause unnecessary errors and bugs.
#' @export
afdf <- function(data){
  as.data.frame(data)
}
