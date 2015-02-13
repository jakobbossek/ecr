#' Generator of the Gauss mutation operator.
#'
#' Default Gauss mutation operator known from Evolutionary Algorithms.
#' @param setOfIndividuals [\code{setOfIndividuals}]\cr
#'   Set of individuals.
#' @param control [\code{list}]\cr
#'   List containing evolutionary operators for fine-tuning the mutation operator.
#'   The \code{gaussMutator} function expects the two values:
#'   \itemize{
#'     \item{mutator.gauss.prob [\code{numeric(1)}]}{Probability of mutation for the gauss mutation operator.}
#'     \item{mutator.gauss.sd [\code{numeric(1)}}{Standard deviance of the Gauss mutation, i. e., the mutation strength.}
#'   }
#' @return [\code{setOfIndividuals}]
#'   Set of individuals.
#' @export
gaussMutator = function(setOfIndividuals, control = list(mutator.gauss.prob = 1, mutator.gauss.sd = 0.05)) {
  n.params = ncol(setOfIndividuals$individuals)
  n = nrow(setOfIndividuals$individuals)

  mutation.bool = matrix(runif(n * n.params) < control$mutator.gauss.prob, ncol = n.params)
  mutation = matrix(0, ncol = n.params, nrow = n)
  idx = which(mutation.bool)
  mutation[idx] = rnorm(length(idx), mean = 0, sd = control$mutator.gauss.sd)
  setOfIndividuals$individuals = setOfIndividuals$individuals + mutation

  return(setOfIndividuals)
}

attr(gaussMutator, "name") = "Gauss mutator"
attr(gaussMutator, "description") = "Adds gaussian noise to each gene"
attr(gaussMutator, "supported") = c("float")
attr(gaussMutator, "class") = c("ecr_operator", "ecr_mutator")
attr(gaussMutator, "defaults") = list(mutator.gauss.prob = 1, mutator.gauss.sd = 0.05)

gaussMutatorCheck = function(operator.control) {
  assertNumber(operator.control$mutator.gauss.prob, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumber(operator.control$mutator.gauss.sd, lower = 0, finite = TRUE, na.ok = FALSE)
}

attr(gaussMutator, "checkFunction") = gaussMutatorCheck
