#' Quick check that an analysis dataset has one row per observation ID
#' @param data A data frame that is supposed to have one row per ID (subject).
#' @param id String naming the ID column. Defaults to \code{id = "subject"}.
#' @details The function checks that the data frame has as many rows as unique
#'   values of the ID column, and that it has at least one row.
#' @export
qc <- function(data, id = "subject"){
  s <- data[, id]

  testthat::expect_equal(nrow(data), length(unique(s)),
                         label = "One row per ID")
  testthat::expect_gt(nrow(data), 0,
                      label = "Data has rows in it")

  invisible()
}
