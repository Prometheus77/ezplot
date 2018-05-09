#' @title Calculate a binomial proportion confidence interval from a sample
#' 
#' @description Uses any of various techniques for calculating a binomial
#'   proportion confidence interval from a sample using the `binom` package
#'
#' @param x Vector of [0, 1] values representing the sample
#' @param method Which method to use to construct the interval. Must be one of
#'   "exact", "ac", "asymptotic", "wilson", "prop.test", "bayes", "logit",
#'   "cloglog", "probit". See `binom` package for details. Default is
#'   "prop.test".
#' @param conf.level The specified confidence level. Defatuls is 0.95.
#' @param na.rm If `TRUE`, NA values will be removed from the sample. If
#'   `FALSE`, NA values will be set to 0. Default is `TRUE`.
calc_binom_ci <- function(x, method = "prop.test", conf.level = 0.95, na.rm = TRUE) {
  checkmate::assert_integer(as.integer(x), lower = 0, upper = 1)
  checkmate::assert_choice(method, choices = c("exact", "ac", "asymptotic", "wilson",
                                               "prop.test", "bayes", "logit", "cloglog",
                                               "probit"))
  checkmate::assert_number(conf.level, lower = 0, upper = 1)
  checkmate::assert_logical(na.rm)
  
  if (na.rm == FALSE) x[is.na(x)] <- 0
  num <- sum(x, na.rm = TRUE)
  denom <- length(x[!is.na(x)])
  
  binom::binom.confint(num, denom, conf.level = conf.level, methods = method)
}