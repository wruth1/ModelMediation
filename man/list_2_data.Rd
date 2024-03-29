% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{list_2_data}
\alias{list_2_data}
\title{Construct a single data frame by \code{rbind}-ing elements of a list. Add/Create group labels.}
\usage{
list_2_data(X_list, group_labels = NULL)
}
\arguments{
\item{X_list}{A list of data frames. Must all have the same number of columns.}

\item{group_labels}{Optionally, a vector of labels for each element of \code{X_list}. Must be either the same length as \code{X_list} or equal to the total number of rows among elements in \code{X_list}. If \code{NULL}, labels are \code{G1}, \code{G2},...}
}
\value{
A data frame containing all elements of \code{X_list} with an extra column for labels.
}
\description{
Construct a single data frame by \code{rbind}-ing elements of a list. Add/Create group labels.
}
\examples{
data = as.data.frame(matrix(c(1,0,0,1), nrow = 2))
more_data = as.data.frame(matrix(c(0,1,1,0), nrow = 2))
data_list = list(data, more_data)

list_2_data(data_list)

data_names = c("Alice", "Bob")
list_2_data(data_list, data_names)
}
