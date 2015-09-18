#' @title
#'   Generator of the Gauss mutation operator.
#'
#' @description
#'   Default Gaussian mutation operator known from Evolutionary Algorithms.
#'
#' @param p [\code{numeric(1)}]\cr
#'   Probability of mutation for the gauss mutation operator.
#' @param sdev [\code{numeric(1)}\cr
#'   Standard deviance of the Gauss mutation, i. e., the mutation strength.
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
makeGaussMutator = function(p = 1L, sdev = 0.05) {
  force(p)
  force(sdev)

  mutatorCheck = function(operator.control) {
    assertNumber(operator.control$p, lower = 0, finite = TRUE, na.ok = FALSE)
    assertNumber(operator.control$sdev, lower = 0, finite = TRUE, na.ok = FALSE)
  }

  defaults = list(p = p, sdev = sdev)
  mutatorCheck(defaults)

  mutator = function(ind, args = defaults, control, task) {
    n.params = length(ind)
    idx = which(runif(n.params) < args$p)
    mut = rnorm(length(idx), mean = 0, sd = args$sdev)
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
