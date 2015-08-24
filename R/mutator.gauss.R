#' @title
#'   Generator of the Gauss mutation operator.
#'
#' @description
#'   Default Gaussian mutation operator known from Evolutionary Algorithms.
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

  mutator = function(ind, args = defaults, control) {
    n.params = length(ind)
    idx = which(runif(n.params) < args$mutator.gauss.prob)
    mut = rnorm(length(idx), mean = 0, sd = args$mutator.gauss.sd)
    ind[idx] = ind[idx] + mut
    # correct bounds
    ind = pmin(pmax(control$par.lower, ind), control$par.upper)
    return(ind)
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
