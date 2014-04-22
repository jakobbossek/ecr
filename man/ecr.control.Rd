\name{ecr.control}
\alias{ecr.control}
\title{Generates control object.}
\usage{
ecr.control(population.size, offspring.size,
  mating.pool.size = floor(population.size/2), representation,
  survival.strategy = "plus", elite.size = 0L, n.params, n.targets = 1L,
  max.iter = 100L, termination.eps = 10^-1, show.info = TRUE,
  show.info.stepsize = 1L, mating.pool.generator = parentSelection,
  generator = makeUniformGenerator(), mutator = gaussMutator,
  recombinator = makeIntermediateRecombinator(), mutator.control = list())
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

  \item{elite.size}{[\code{integer(1)}]\cr Number of
  fittest individuals of the current generation that shall
  be copied to the next generation without changing.
  Default is 0. Keep in mind, that the algorithm does not
  care about this option if the \code{survival.strategy} is
  set to 'plus'.}

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

  \item{generator}{[\code{ecr_generator}]\cr Generator
  operator of type \code{ecr_generator} for the generation
  of the initial population.}

  \item{mutator}{[\code{ecr_mutator}]\cr Mutation operator
  of type \code{ecr_mutator}.}

  \item{recombinator}{[\code{ecr_recombinator}]\cr
  Recombination operator of type \code{ecr_recombinator}.}

  \item{mutator.control}{[\code{list}]\cr List of
  evolutionary parameters for the corresponding mutation
  operator. See the help pages for the mutation operators
  for the needed values.}
}
\value{
S3 object of type \code{ecr.control}.
}
\description{
The ecr package offers a framework for evolutionary
computing and therefore offers a lot of customization
options. The control object is a simple but powerful
wrapper for all these options and sets convenient default
options.
}

