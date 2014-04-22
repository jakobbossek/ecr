\name{gaussMutator}
\alias{gaussMutator}
\title{Generator of the Gauss mutation operator.}
\usage{
gaussMutator(setOfIndividuals, control = list(mutator.gauss.prob = 1,
  mutator.gauss.sd = 0.05))
}
\arguments{
  \item{setOfIndividuals}{[\code{setOfIndividuals}]\cr Set
  of individuals.}

  \item{control}{[\code{list}]\cr List containing
  evolutionary operators for fine-tuning the mutation
  operator.  The \code{gaussMutator} function expects the
  two values: \itemize{ \item{mutator.gauss.prob
  [\code{numeric(1)}]}{Probability of mutation for the
  gauss mutation operator.} \item{mutator.gauss.sd
  [\code{numeric(1)}}{Standard deviance of the Gauss
  mutation, i. e., the mutation strength.} }}
}
\value{
[\code{setOfIndividuals}] Set of individuals.
}
\description{
Default Gauss mutation operator known from Evolutionary
Algorithms.
}

