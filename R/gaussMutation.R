#' Generator of the Gauss mutation operator.
#'
#' Default Gauss mutation operator known from Evolutionary Algorithms.
#'
#' @return [\code{esso_mutator}]
#'   Gaussian mutation operator.
#' @export
makeGaussMutator = function() {
  gaussMutator = function(setOfIndividuals, control = list(mutator.gauss.prob = 1, mutator.gauss.sd = 0.05)) {
    n.params = ncol(setOfIndividuals$population)
    n = nrow(setOfIndividuals$population)
    for (i in seq(n)) {
      mutation.bool = (runif(n.params) <= control$mutator.gauss.prob)
      mutation = ifelse(mutation.bool, rnorm(1, mean = 0, sd = control$mutator.gauss.sd), 0)
      setOfIndividuals$population[i, ] = setOfIndividuals$population[i, ] + mutation
    }
    return(setOfIndividuals)
  }

  makeMutator(
    mutator = gaussMutator,
    name = "Gauss Mutator",
    supported = c("float")
  )
}
