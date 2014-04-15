\name{esoo}
\alias{esoo}
\title{Working horse of the esoo package.}
\usage{
esoo(objective.fun, control, global.optimum = NA, lower = NA, upper = NA)
}
\arguments{
  \item{objective.fun}{[\code{function}]\cr Target
  function.}

  \item{control}{[\code{esoo.control}]\cr Control object.}

  \item{global.optimum}{[\code{numeric}]\cr Parameter
  combination of the global optimum of the fun. This
  parameter is optional.}

  \item{lower}{[\code{numeric}]\cr Lower box constraints
  for the parameters. These are needed, if representation
  type is set to 'float'.}

  \item{upper}{[\code{numeric}]\cr Upper box constraints
  for the parameters. These are needed, if representation
  type is set to 'float'. return [\code{esooResult}] Object
  of type \code{esooResult} containing a list: \itemize{
  \item{best.param \code{numeric}}{Best parameter
  combination.} \item{best.value \code{numeric(1)}}{Best
  reached value.} \item{trace
  \code{esooTrace}}{Optimization path.} }}
}
\description{
Takes a function and searches for global optimum with an
evolutionary approach.
}

