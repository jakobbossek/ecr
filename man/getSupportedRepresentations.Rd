\name{getSupportedRepresentations}
\alias{getSupportedRepresentations}
\title{Returns the representations which a specific operator supports.}
\usage{
getSupportedRepresentations(operator)
}
\arguments{
  \item{operator}{[\code{ecr_operator}]\cr Operator
  object.}
}
\value{
[\code{character}] Vector of representation types.
}
\description{
Operators and representation types are not mandatory
compatible. This function detemines the types an operator
can operate on.
}

