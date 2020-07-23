#' Compute a complete set of summary statistics for continuous variables
#' @param data A data.frame containing numeric values and columns for visit and arm, and
#'   optionally containing columns for domain and test.
#' @param value,domain,test,arm,visit Unquoted names of columns in \code{data}
#'   to be used in summarizing. They all default to their own names.
#' @param ci Whether to report the 95% confidence interval for the mean or geometric
#'   mean. Defaults to \code{ci = FALSE}.
#' @param geometric Whether to report the geometric rather than arithmetic mean (and
#'   confidence interval). Defaults to \code{geometrci = FALSE}.
#' @details Note
#'   that if 'domain' and 'test' do not exist, they will be set to empty strings.
#'   If you want a table of all baseline values, not split by arm, you need
#'   to filter down to the correct visit and replace arm with a single value
#'   before calling this function. Also NOTE THAT you need to ensure BEFOREHAND that
#'   missing values are properly included (possibly via \code{dplyr::complete}).
#'   The confidence interval is fixed (currently) at 95% and uses the Gaussian
#'   approximation.
#' @return A data frame.
#' @example fs <- filter(bm, visit=="Baseline") %>%
#'             mutate(test=abbrev, arm="all") %>%
#'             fullSummary()
#' @note In a sane world, you'd add another line to the pipeline and remove Mean
#'   and SD. Also, I ought to write kable and formattable methods. Earlier
#'   versions (prior to 2019-05-22) used strings as all arguments but the first.
#'   However, dplyr is getting noiser about that, so I switched.
#' @export
fullSummary <- function (data, value = value, domain = domain, test = test,
                         arm = arm, visit = visit, ci = FALSE, geometric = FALSE,
                         zeros = NULL){

  value <- enquo(value)
  domain <- enquo(domain)
  test <- enquo(test)
  arm <- enquo(arm)
  visit <- enquo(visit)

  if (!(quo_name(domain) %in% names(data))) {
    data[, quo_name(domain)] <- ""
  }

  if (!(quo_name(visit) %in% names(data))) {
    data[, quo_name(visit)] <- ""
  }
  if (quo_name(value) != "value") {
    data$value <- data[, quo_name(value)]
  }

  checkGeometric(data$value, geometric, zeros)

  data <- doTransform(data, !!domain, !!test, geometric = geometric, zeros = zeros)

  res <- dplyr::group_by(data, !!domain, !!test, !!arm, !!visit) %>%
    dplyr::summarize(N = length(value), Missing = sum(is.na(value)),
                     Min. = min(value, na.rm = TRUE),
                     Q1 = quantile(value, 0.25, na.rm = TRUE),
                     Median = median(value, na.rm = TRUE),
                     Q3 = quantile(value, 0.75, na.rm = TRUE),
                     Max. = max(value, na.rm = TRUE),
                     Mean = mean(value, na.rm = TRUE),
                     SD = sd(value, na.rm = TRUE),
                     Gmean = hilo(..tvalue.., which = "mean"),
                     Lo.95 = hilo(..tvalue.., which = "lo"),
                     Hi.95 = hilo(..tvalue.., which = "hi")) %>%
    as.data.frame(stringsAsFactors = FALSE)

  if (geometric){
    res <- select(res, -Mean, -SD)
  } else {
    res <- select(res, -Gmean)
  }

  if (!ci){
    res <- select(res, -Lo.95, -Hi.95)
  }

  res
}

