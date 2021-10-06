#' Calculate Power for Cox Regression Model
#'
#' Compute power of Cox proportional hazards model or determine parameters to
#' obtain target power.
#'
#' @param hr Hazard ratio for a one unit increase in the predictor of interest
#' @param eventprob Probability that an uncensored event occurs
#' @param rsquare The percent of variation in the predictor of interest
#'   explained by other covariates expected to be adjusted for in the Cox
#'   regression model
#' @param stddev Standard deviation of the predictor of interest
#' @param n Sample size
#' @param sig_level Significance level. Default = 0.05
#' @param power Power of the test
#' @param alternative Character. The alternative hypothesis of the test. Must be
#'   "two.sided" (Default), "greater", or "less"
#'
#' @details
#' Exactly one of the parameters `n` or `power` must be passed as `NULL` -- that
#' parameter is determined from the others.
#'
#' @return
#' A list containing:
#' * `hr`: The Hazard Ratio
#' * `eventprob`: The probability that an uncensored event occurs
#' * `nevents`: The total number of events needed (equal to `eventprob` x `n`)
#' * `n`: The total sample size needed
#' * `sig_level`: The signficance level
#' * `power`: The power of the test
#' * `alternative`: The alternative hypothesis of the test
#'
#' @references
#' Hsieh, FY, and Philip W Lavori. 2000. "Sample-Size Calculations for the
#' Cox Proportional Hazards Regression Model with Nonbinary Covariates."
#' Controlled Clinical Trials 21 (6): 552–60.
#'
#' Latouche, Aurélien, Raphaël Porcher, and Sylvie Chevret. 2004. "Sample Size
#' Formula for Proportional Hazards Modelling of Competing Risks." Statistics in
#' Medicine 23 (21): 3263–74.
#'
#' Schoenfeld, David A. 1983. "Sample-Size Formula for the Proportional-Hazards
#' Regression Model." Biometrics, 499–503.
#' @export
#'
#' @examples
#' ## specify n to output the power
#' pwr_coxph(1.5, 0.8, 0.2, 1.1, n = 80)
#'
#' ## specify power to output the sample size
#' pwr_coxph(1.5, 0.8, 0.2, 1.1, power = 0.8)

pwr_coxph <- function(hr = NULL, eventprob = NULL, rsquare = NULL,
                      stddev = NULL, n = NULL, sig_level = 0.05, power = NULL,
                      alternative = c("two.sided", "less", "greater")) {

  if (sum(sapply(list(power, n), is.null)) != 1) {
    stop("Exactly one of n and power must be NULL")
  }

  alternative <- match.arg(alternative)

  if (is.null(power)) {
    if (alternative == "greater") {
      power <- 1 - get_power(hr, eventprob, rsquare, stddev, n, sig_level = 1 - sig_level)
    } else if (alternative == "less") {
      power <- get_power(hr, eventprob, rsquare, stddev, n, sig_level)
    } else if (alternative == "two.sided") {
      power <- 1 - get_power(hr, eventprob, rsquare, stddev, n, sig_level = 1 - (sig_level / 2)) +
        get_power(hr, eventprob, rsquare, stddev, n, sig_level = (sig_level / 2))
    }
  }

  if (is.null(n)) {
    if (alternative == "greater") {
      n <- get_n_onesided(hr, eventprob, rsquare, stddev, sig_level = 1 - sig_level, power)
    } else if (alternative == "less") {
      n <- get_n_onesided(hr, eventprob, rsquare, stddev, sig_level, power)
    } else if (alternative == "two.sided") {
      n <- stats::uniroot(function(n) {
        1 - get_power(hr, eventprob, rsquare, stddev, n, sig_level = 1 - (sig_level / 2)) +
          get_power(hr, eventprob, rsquare, stddev, n, sig_level = (sig_level / 2))
      } - power, c(2, 1e+09))$root
    }
  }
  return(list(hr = hr, eventprob = eventprob, nevents = n * eventprob,
              n = n, sig_level = sig_level,
              power = power, alternative = alternative))
}

get_power <- function(hr, eventprob, rsquare, stddev, n, sig_level) {
  stats::pnorm(stats::qnorm(sig_level, lower.tail = TRUE) - stddev *
                 sqrt(n * eventprob * (1 - rsquare)) * log(hr),
               lower.tail = TRUE)
}

get_n_onesided <- function(hr, eventprob, rsquare, stddev, sig_level, power) {
  ((stats::qnorm(power, lower.tail = TRUE) + stats::qnorm(1 - sig_level))^2) /
    (eventprob * (1 - rsquare) * stddev^2 * log(hr)^2)
}
