% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_growth_curve_fit.R
\name{growth_curve_fit}
\alias{growth_curve_fit}
\title{growth_curve_fit}
\usage{
growth_curve_fit(data, trait, type = "logistic")
}
\arguments{
\item{data}{genotype time series data must contain 'genotype', 'timePoint',
'timeNumber', and specified trait columns.}

\item{trait}{character string indicating the trait.}

\item{type}{character string indicating the type of growth curve. 'linear' or
'logistic'. Default = 'logistic'}
}
\value{
The return value, if any, from executing the function.
}
\description{
fit growth curve to genotype time series data
}
