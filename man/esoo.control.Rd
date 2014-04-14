\name{esoo.control}
\alias{esoo.control}
\title{Generates control object.}
\usage{
esoo.control(population.size, offspring.size, representation, n.params,
  n.targets = 1L, max.iter = 100L, show.info = TRUE,
  show.info.stepsize = 1L, mutator = makeGaussMutator(),
  recombinator = makeIntermediateRecombinator(), mutator.gauss.prob = 1,
  mutator.gauss.sd = 0.05)
}
\arguments{
  \item{population.size}{[\code{integer(1)}]\cr Number of
  individuals in the population.}

  \item{offspring.size}{[\code{integer(1)}]\cr Number of
  idividuals generated in each generation.}

  \item{representation}{[\code{character(1)}]\cr Genotype
  representation of the parameters. Available are binary,
  real, integer and permutation.}

  \item{n.params}{[\code{integer(1)}]\cr Number of
  parameters of the objective function.}

  \item{n.targets}{[\code{integer(1)}]\cr Number of target
  functions. Default is \code{1}. For bicriteria fitness
  functions this should therefore be set to \code{2}.}

  \item{max.iter}{[\code{integer(1)}]\cr Maximum number of
  generations. This is one possible stopping criterion.}

  \item{show.info}{[\code{logical(1)}]\cr Logical flag
  indicating whether helpful information should be printed
  during the evolutionary process.}

  \item{show.info.stepsize}{[\code{integer(1)}]\cr This
  positive value indicates after which iterations output
  shall be presented.}

  \item{mutator}{[\code{esoo_mutator}]\cr Mutation operator
  of type \code{esoo_mutator}.}

  \item{recombinator}{[\code{esoo_recombinator}]\cr
  Recombination operator of type \code{esoo_recombinator}.}

  \item{mutator.gauss.prob}{[\code{numeric(1)}]\cr
  Probability of mutation for the gauss mutation operator.}

  \item{mutator.gauss.sd}{[\code{numeric(1)}]\cr Standard
  deviance of the Gauss mutation, i. e., the mutation
  strength.}
}
\value{
S3 object of type \code{esoo.control}.
}
\description{
The esoo package offers a framework for evolutionary
computing and therefore offers a lot of customization
options. The control object is a simple but powerful
wrapper for all these options and sets convenient default
options.
}

