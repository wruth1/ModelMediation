% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bootstrap_Samplers.R
\name{one_semi_parametric_sample}
\alias{one_semi_parametric_sample}
\title{Generate a semiparametric bootstrap sample using the provided fitted models}
\usage{
one_semi_parametric_sample(mod_Y, mod_M)
}
\arguments{
\item{mod_Y}{A model fit using \code{glmer} to predict \code{Y}}

\item{mod_M}{A model fit using \code{glmer} to predict \code{M}}
}
\value{
A data frame obtained by generating bootstrap samples independently across groups.
}
\description{
Generate a semiparametric bootstrap sample using the provided fitted models
}
\examples{
n = 20
K = 3
all_reg_pars = make_all_reg_pars()
data = make_validation_data(n, K, all_reg_pars)

mod_Y = fit_mod_Y(data)
mod_M = fit_mod_M(data)

one_semi_parametric_sample(mod_Y, mod_M)
}
