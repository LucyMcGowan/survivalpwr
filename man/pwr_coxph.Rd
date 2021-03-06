% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pwr_coxph.R
\name{pwr_coxph}
\alias{pwr_coxph}
\title{Calculate Power for Cox Regression Model}
\usage{
pwr_coxph(
  hr = NULL,
  eventprob = NULL,
  n = NULL,
  rsquare = 0,
  stddev = 0.5,
  sig_level = 0.05,
  power = NULL,
  alternative = c("two.sided", "less", "greater")
)
}
\arguments{
\item{hr}{Hazard ratio for a one unit increase in the predictor of interest}

\item{eventprob}{Probability that an uncensored event occurs}

\item{n}{Sample size}

\item{rsquare}{The percent of variation in the predictor of interest
explained by other covariates expected to be adjusted for in the Cox
regression model (Default = 0)}

\item{stddev}{Standard deviation of the predictor of interest (Default = 0.5)}

\item{sig_level}{Significance level. (Default = 0.05)}

\item{power}{Power of the test}

\item{alternative}{Character. The alternative hypothesis of the test. Must be
"two.sided" (Default), "greater", or "less"}
}
\value{
Object of class "\code{power.htest}", a list containing the parameters specified
as well as the one computed.
}
\description{
Compute power of Cox proportional hazards model or determine parameters to
obtain target power.
}
\details{
Exactly one of the parameters \code{n} or \code{power} must be passed as \code{NULL} -- that
parameter is determined from the others.
}
\examples{
## specify n to output the power
pwr_coxph(1.5, 0.8,  n = 80)

## specify power to output the sample size
pwr_coxph(1.5, 0.8, power = 0.8)
}
\references{
Hsieh, FY, and Philip W Lavori. 2000. "Sample-Size Calculations for the
Cox Proportional Hazards Regression Model with Nonbinary Covariates."
Controlled Clinical Trials 21 (6): 552–60.

Latouche, Aurélien, Raphaël Porcher, and Sylvie Chevret. 2004. "Sample Size
Formula for Proportional Hazards Modelling of Competing Risks." Statistics in
Medicine 23 (21): 3263–74.

Schoenfeld, David A. 1983. "Sample-Size Formula for the Proportional-Hazards
Regression Model." Biometrics, 499–503.
}
