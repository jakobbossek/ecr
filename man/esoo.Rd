\name{esoo}
\alias{esoo}
\title{Working horse of the esoo package.}
\usage{
esoo(f, max.iter, pop.size)
}
\arguments{
  \item{f}{[\code{function}]\cr Target function.}

  \item{max.iter}{[\code{integer(1)}]\cr Maximum number of
  iterations.}

  \item{pop.size}{[\code{integer(1)}]\cr Population size.
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
\examples{
\dontrun{
library(soobench)
f = generate_sphere_function(2)
res = esoo(f, max.iter = 50, pop.size = 10)
print(res$best.value)
}
}

