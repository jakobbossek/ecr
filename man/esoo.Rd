\name{esoo}
\alias{esoo}
\title{Working horse of the esoo package.}
\usage{
esoo(f, control)
}
\arguments{
  \item{f}{[\code{function}]\cr Target function.}

  \item{control}{[\code{esoo.control}]\cr Control object.
  return [\code{esooResult}] Object of type
  \code{esooResult} containing a list: \itemize{
  \item{best.param \code{numeric}}{Best parameter
  combination.} \item{best.value \code{numeric(1)}}{Best
  reached value.} \item{trace
  \code{esooTrace}}{Optimization path.} }}
}
\description{
Takes a function and searches for global optimum with an
evolutionary approach.
}

