% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{commandArgs.def}
\alias{commandArgs.def}
\title{commandArgs.def}
\usage{
commandArgs.def(defaults, error.message = NULL, ...)
}
\arguments{
\item{defaults}{a list of defaults, NA when no default is set}

\item{error.message}{Show this error message and stop if not null. Defaults NULL.}
}
\description{
A version of commandArgs function with default values
}
\examples{
args <- commandArgs.def(c(NA, 2), error.message='This program takes two input. The second input defaults to 2 if not supplied')
}
