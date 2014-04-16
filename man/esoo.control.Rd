\name{esoo.control}
\alias{esoo.control}
\title{Generates control object.}
\usage{
esoo.control(population.size, offspring.size,
  mating.pool.size = floor(population.size/2), representation,
  survival.strategy = "plus", n.params, n.targets = 1L, max.iter = 100L,
  termination.eps = 10^-1, show.info = TRUE, show.info.stepsize = 1L,
  mating.pool.generator = parentSelection,
  generator = makeUniformGenerator(), mutator = makeGaussMutator(),
  recombinator = makeIntermediateRecombinator(), mutator.gauss.prob = 1,
  mutator.gauss.sd = 0.05)
}
\arguments{
  \item{population.size}{[\code{integer(1)}]\cr Number of
  individuals in the population.}

  \item{offspring.size}{[\code{integer(1)}]\cr Number of
  individuals generated in each generation.}

  \item{mating.pool.size}{[\code{integer(1)}]\cr Number of
  individuals which can potentially participate in the
  generation of offspring. Default is half of the
  population size.}

  \item{representation}{[\code{character(1)}]\cr Genotype
  representation of the parameters. Available are binary,
  real, integer and permutation.}

  \item{survival.strategy}{[\code{character(1)}]\cr
  Determines the survival strategy used by the EA. Possible
  are 'plus' for a classical (mu + lambda) strategy and
  'comma' for (mu, lambda).}

  \item{n.params}{[\code{integer(1)}]\cr Number of
  parameters of the objective function.}

  \item{n.targets}{[\code{integer(1)}]\cr Number of target
  functions. Default is \code{1}. For bicriteria fitness
  functions this should therefore be set to \code{2}.}

  \item{max.iter}{[\code{integer(1)}]\cr Maximum number of
  generations. This is one possible stopping criterion.}

  \item{termination.eps}{[\code{numeric(1)}]\cr The
  optimization process will stop if the gap between known
  optimum and current best individual falls below this
  threshold value.}

  \item{show.info}{[\code{logical(1)}]\cr Logical flag
  indicating whether helpful information should be printed
  during the evolutionary process.}

  \item{show.info.stepsize}{[\code{integer(1)}]\cr This
  positive value indicates after which iterations output
  shall be presented.}

  \item{mating.pool.generator}{[\code{function}]\cr
  Generator operator which implements a procedure to copy
  individuals from a given population to the mating pool,
  i. e., allow them to become parents.}

  \item{generator}{[\code{esoo_generator}]\cr Generator
  operator of type \code{esoo_generator} for the generation
  of the initial population.}

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

