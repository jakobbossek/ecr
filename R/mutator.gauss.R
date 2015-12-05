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
  assertNumber(p, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumber(sdev, lower = 0, finite = TRUE, na.ok = FALSE)

  force(p)
  force(sdev)

  mutator = function(ind, task, control) {
    n.params = length(ind)
    idx = which(runif(n.params) < p)
    mut = rnorm(length(idx), mean = 0, sd = sdev)
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
    params = list(p = p, sdev = sdev)
  )
}
