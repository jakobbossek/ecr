\name{generateRandomInitialPopulation}
\alias{generateRandomInitialPopulation}
\title{Generates an initial population.}
\usage{
generateRandomInitialPopulation(size, n.params, lower.bounds, upper.bounds)
}
\arguments{
  \item{size}{[\code{integer(1)}]\cr Number of points to
  generate.}

  \item{n.params}{[\code{integer(1)}]\cr Number of
  parameter of the target function.}

  \item{lower.bounds}{[\code{numeric}]\cr Vector of size
  \code{n.params} indicating the lower bounds for each
  dimension.}

  \item{upper.bounds}{[\code{numeric}]\cr Vector of size
  \code{n.params} indicating the upper bounds for each
  dimension.}
}
\value{
[\code{setOfIndividuals}]
}
\description{
Samples uniformally distributed points in the design space
of the target function taking care not to violate bounds.
}

