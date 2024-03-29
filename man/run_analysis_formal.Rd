% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{run_analysis_formal}
\alias{run_analysis_formal}
\title{Generate bootstrap samples, compute estimates, and construct CIs. Data must be formatted as a formal data frame.}
\usage{
run_analysis_formal(data_formal, B, .parallel = FALSE, .verbose = FALSE)
}
\arguments{
\item{data_formal}{A formal data frame.}

\item{B}{Number of bootstrap replicates.}

\item{.parallel}{Should bootstrapping be performed in parallel?}

\item{.verbose}{Should progress bars be produced for the two bootstrap analyses?}
}
\value{
A data frame containing confidence intervals for each mediation effect, in each group, for all flavours of bootstrap and bootstrap CI.
}
\description{
Generate bootstrap samples, compute estimates, and construct CIs. Data must be formatted as a formal data frame.
}
\examples{
1+1
}
