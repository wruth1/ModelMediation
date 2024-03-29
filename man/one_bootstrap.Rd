% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bootstrap_Logic.R
\name{one_bootstrap}
\alias{one_bootstrap}
\title{Perform a single bootstrap analysis}
\usage{
one_bootstrap(
  data = NULL,
  mod_Y = NULL,
  mod_M = NULL,
  boot_type = c("par", "npar", "spar"),
  .careful = TRUE
)
}
\arguments{
\item{data}{Original dataset. Optional for parametric (as long as \code{mod_Y} and \code{mod_M} are supplied), required for non-parametric, .}

\item{mod_Y}{Regression model for predicting \code{Y}. Recommended but optional for parametric (as long as \code{data} is supplied), not used for non-parametric.}

\item{mod_M}{Regression model for predicting \code{M}. Recommended but optional for parametric (as long as \code{data} is supplied), not used for non-parametric.}

\item{boot_type}{Indicates which type of bootstrap should be performed. Options include parametric (\code{par}), non-parametric (\code{npar}), and semi-parametric (\code{spar}).}

\item{.careful}{Should input be checked for whether enough information has been provided for the requested flavour of bootstrap? Recommended setting is \code{TRUE} if running interactively and \code{FALSE} if running inside another function which has already checked.}
}
\value{
One component of standard bootstrap output.
}
\description{
Perform a single bootstrap analysis
}
\examples{
n = 20
K = 3
all_reg_pars = make_all_reg_pars()
data = make_validation_data(n, K, all_reg_pars)

mod_Y = fit_mod_Y(data)
mod_M = fit_mod_M(data)

\dontrun{one_bootstrap(mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .careful = TRUE)}

one_bootstrap(data = data, boot_type = "npar", .careful = TRUE)
}
