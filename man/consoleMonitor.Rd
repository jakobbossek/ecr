\name{consoleMonitor}
\alias{consoleMonitor}
\title{Simple monitoring function.}
\usage{
consoleMonitor(objective.fun, population, trace, iter, control)
}
\arguments{
  \item{objective.fun}{[\code{function}]\cr Objective
  function.}

  \item{population}{[\code{setOfIndividuals}]\cr Current
  population.}

  \item{trace}{[\code{ecr_trace}]\cr Optimization trace.}

  \item{iter}{[\code{integer(1)}]\cr Current iteration.}

  \item{control}{[\code{ecr_control}]\cr Control object.}
}
\description{
This is the default monitoring function used by ecr. It
simply outputs the iteration as well as minimal, mean and
maximal target values from the current population.
}

