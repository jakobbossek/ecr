\name{extractParamSetFromSOOFunction}
\alias{extractParamSetFromSOOFunction}
\title{Generates ParamSet object out of a given soobench function.}
\usage{
extractParamSetFromSOOFunction(fn)
}
\arguments{
  \item{fn}{[\code{\link[soobench]{soo_function}}]\cr
  Source function.}
}
\value{
[\code{\link[ParamHelpers]{ParamSet}}] Parameter set of
type \code{\link[ParamHelpers]{ParamSet}}
}
\description{
Generates ParamSet object out of a given soobench function.
}
\examples{
library(soobench)
fn = ackley_function(8)
par.set = extractParamSetFromSOOFunction(fn)
print(par.set)
}

