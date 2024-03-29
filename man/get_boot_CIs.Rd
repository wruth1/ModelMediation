% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Confidence_Intervals.R
\name{get_boot_CIs}
\alias{get_boot_CIs}
\title{Construct bootstrap confidence intervals}
\usage{
get_boot_CIs(
  boot_reg_coeffs,
  type = c("percentile", "basic"),
  fitted_med_effs = NULL,
  mod_Y = NULL,
  mod_M = NULL
)
}
\arguments{
\item{boot_reg_coeffs}{A bootstrap sample of regression coefficients}

\item{type}{Which type(s) of confidence interval should be constructed? Currently only \code{"percentile"} and \code{"basic"} are supported.}

\item{fitted_med_effs}{Data frame of mediation effects obtained from the observed data (i.e. not from any bootstrap sample).}

\item{mod_Y, mod_M}{Regression models fit to the observed data (i.e. not from any bootstrap sample) for predicting the outcome and mediators respectively.}
}
\value{
A data frame containing bootstrap CIs for each group and mediation effect. Columns include:
\itemize{
\item Group label (\code{group})
\item Type of mediation effect (\code{med_type}, values include \code{de}, \code{ie} and \code{te})
\item Type of confidence interval (\code{CI_type}, values include \code{pct} and \code{bas})
\item Lower and upper confidence limits (\code{lcl} and \code{ucl} respectively)
\item If available, the point estimate for each mediation effect on each group (\code{estimate}, repeated for the two types of interval if both are requested)
}
}
\description{
Construct percentile or basic bootstrap confidence intervals (or both) from the provided bootstrap sample. Note: If basic intervals are requested then either the mediation effects or regression models fit the the observed data must be supplied.

Design note: I allow fitted effects from the observed data to be passed as mediation effects but not as regression coefficients. This avoids any ambiguity about whether regression coefficients can be a raw dump from \code{mod_Y} and \code{mod_M}. I can imagine relaxing this requirement, but it would require more input checking and I don't want to do that right now.
}
\examples{
1+1
}
