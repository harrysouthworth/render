#' Compute a complete set of summary statistics for continuous variables
#' @param data A data.frame containing numeric values and columns for visit and arm, and
#'   optionally containing columns for domain and test.
#' @param value,domain,test,arm,visit Unquoted names of columns in \code{data}
#'   to be used in summarizing. They all default to their own names.
#' @param ci Whether to report the 95% confidence interval for the mean or geometric
#'   mean. Defaults to \code{ci = FALSE}.
#' @param pvalue Whether to report the p-value for the mean being different from
#'   zero. Almost always, this will be purely descriptive because comparison
#'   to baseline is not the point of a clinical trial.
#' @param pvalue.digits The number of decimal places at which to round and format
#'   p-values. Defaults to \code{pvalue.digits = 4}. To avoid formatting, specify
#'   \code{pvalue.digts = NULL}.
#' @param geometric Whether to report the geometric rather than arithmetic mean (and
#'   confidence interval). Defaults to \code{geometrci = FALSE}.
#' @param zeros If geometric means are required but the data contain 0s, the user
#'   needs to tell the function what to do: either \code{zeros = "add1"} will use
#'   \code{log1p} instead of \code{log}; \code{zeros = "omit"} will use the logs,
#'   omitting any -Inf values from the calculations. No default is provided because
#'   the user needs to know there 0s are there, and how to deal with them is
#'   arbitrary.
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
#'   However, dplyr is getting noiser about that, so I switched. I've added
#'   computation of intervals and p-values: I don't like it, but keep getting
#'   asked and it's better to build it in so that it can be properly tested,
#'   rather than do it on the fly and risk making mistakes.
#' @export
fullSummary <- function (data, value = value, domain = domain, test = test,
                         arm = arm, visit = visit, ci = FALSE, pvalue = FALSE,
                         pvalue.digits = 4,
                         approx = "t", alpha = .05, geometric = FALSE, zeros = NULL){

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

  zeros <- checkGeometric(data$value, geometric, zeros)

  data <- doTransform(data, as_label(domain), as_label(test),
                      as_label(arm), as_label(visit),
                      geometric = geometric, zeros = zeros)

  res <- dplyr::group_by(data, !!domain, !!test, !!arm, !!visit) %>%
    dplyr::summarize(N = length(value), Missing = sum(is.na(value)),
                     Min. = min(value, na.rm = TRUE),
                     Q1 = quantile(value, 0.25, na.rm = TRUE),
                     Median = median(value, na.rm = TRUE),
                     Q3 = quantile(value, 0.75, na.rm = TRUE),
                     Max. = max(value, na.rm = TRUE),
                     Mean = mean(value, na.rm = TRUE),
                     SD = sd(value, na.rm = TRUE),
                     Gmean = hilo(..tvalue.., which = "mean", geometric, zeros, alpha, approx),
                     Lo = hilo(..tvalue.., which = "lo", geometric, zeros, alpha, approx),
                     Hi = hilo(..tvalue.., which = "hi", geometric, zeros, alpha, approx),
                     `p-value` = hilo(..tvalue.., which = "p-value", geometric, zeros, alpha, approx)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  if (geometric){
    res <- select(res, -Mean, -SD)
  } else {
    res <- select(res, -Gmean)
  }

  if (!ci){
    res <- select(res, -Lo, -Hi)
  }

  if (!pvalue){
    res <- select(res, -`p-value`)
  } else if (!is.null(pvalue.digits)){
    lts <- paste0("<", 10^(-pvalue.digits))
    fmt <- paste0("%.", pvalue.digits, "f")
    res <- mutate(res, `p-value` = ifelse(`p-value` < 10^(-pvalue.digits), lts, sprintf(fmt, `p-value`)))
  }

  res
}

