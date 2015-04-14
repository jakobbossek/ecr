#' Generator of the Gauss mutation operator.
#'
#' Default Gauss mutation operator known from Evolutionary Algorithms.
#'
#' @param mutator.gauss.prob [\code{numeric(1)}]\cr
#'   Probability of mutation for the gauss mutation operator.
#' @param mutator.gauss.sd [\code{numeric(1)}\cr
#'   Standard deviance of the Gauss mutation, i. e., the mutation strength.
#' @return [\code{ecr_mutator}]
#' @export
makeGaussMutator = function(mutator.gauss.prob = 1L, mutator.gauss.sd = 0.05) {
  force(mutator.gauss.prob)
  force(mutator.gauss.sd)

  mutatorCheck = function(operator.control) {
    assertNumber(operator.control$mutator.gauss.prob, lower = 0, finite = TRUE, na.ok = FALSE)
    assertNumber(operator.control$mutator.gauss.sd, lower = 0, finite = TRUE, na.ok = FALSE)
  }

  defaults = list(mutator.gauss.prob = mutator.gauss.prob, mutator.gauss.sd = mutator.gauss.sd)
  mutatorCheck(defaults)

  mutator = function(setOfIndividuals, control = defaults) {
    inds = setOfIndividuals$individuals
    n.params = length(inds[[1]])
    n = length(inds)

    for (i in seq(n)) {
      idx = which(runif(n.params) < control$mutator.gauss.prob)
      mut = rnorm(length(idx), mean = 0, sd = control$mutator.gauss.sd)
      inds[[i]][idx] = inds[[i]][idx] + mut
    }
    setOfIndividuals$individuals = inds
    return(setOfIndividuals)
  }

  makeMutator(
    mutator = mutator,
    name = "Gauss mutator",
    description = "Adds gaussian noise to each gene",
    supported = "float",
    defaults = defaults,
    checker = mutatorCheck
  )
}
