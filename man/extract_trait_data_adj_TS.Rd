% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_extract_trait_data_adj_TS.R
\name{extract_trait_data_adj_TS}
\alias{extract_trait_data_adj_TS}
\title{extract_trait_data_adj_TS}
\usage{
extract_trait_data_adj_TS(
  data,
  tp_sel,
  trait_name,
  new_name = NULL,
  keep = "genotype"
)
}
\arguments{
\item{data}{data.frame containing adjusted traits after the fitting of
spatial_adjustment function.}

\item{tp_sel}{numeric vector of selected timepoints.}

\item{trait_name}{character string indicating the name of the selected trait}

\item{new_name}{optional new name of the selected trait.}

\item{keep}{column from data that should be kept.}
}
\value{
The return value, if any, from executing the function.
}
\description{
Etract trait value from a long format adjusted phenotype data frame
and return them in a wide format.
}
