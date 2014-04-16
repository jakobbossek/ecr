\name{ecr}
\alias{ecr}
\title{Working horse of the ecr package.}
\usage{
ecr(objective.fun, control, global.optimum = NA, lower = NA, upper = NA)
}
\arguments{
  \item{objective.fun}{[\code{function}]\cr Target
  function.}

  \item{control}{[\code{ecr.control}]\cr Control object.}

  \item{global.optimum}{[\code{numeric}]\cr Parameter
  combination of the global optimum of the fun. This
  parameter is optional.}

  \item{lower}{[\code{numeric}]\cr Lower box constraints
  for the parameters. These are needed, if representation
  type is set to 'float'.}

  \item{upper}{[\code{numeric}]\cr Upper box constraints
  for the parameters. These are needed, if representation
  type is set to 'float'. return [\code{ecrResult}] Object
  of type \code{ecrResult} containing a list: \itemize{
  \item{best.param \code{numeric}}{Best parameter
  combination.} \item{best.value \code{numeric(1)}}{Best
  reached value.} \item{trace \code{ecrTrace}}{Optimization
  path.} }}
}
\description{
Takes a function and searches for global optimum with an
evolutionary approach.
}

