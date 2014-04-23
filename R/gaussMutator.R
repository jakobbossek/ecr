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
  n.params = ncol(setOfIndividuals$population)
  n = nrow(setOfIndividuals$population)
  for (i in seq(n)) {
    mutation.bool = (runif(n.params) <= control$mutator.gauss.prob)
    mutation = ifelse(mutation.bool, rnorm(1, mean = 0, sd = control$mutator.gauss.sd), 0)
    setOfIndividuals$population[i, ] = setOfIndividuals$population[i, ] + mutation
  }
  return(setOfIndividuals)
}

attr(gaussMutator, "name") = "Gauss mutator"
attr(gaussMutator, "description") = "Adds gaussian noise to each gene"
attr(gaussMutator, "supported") = c("float")
attr(gaussMutator, "class") = c("ecr_operator", "ecr_mutator")
attr(gaussMutator, "defaults") = list(mutator.gauss.prob = 1, mutator.gauss.sd = 0.05)

gaussMutatorCheck = function(operator.control) {
  checkArg(operator.control$mutator.gauss.prob, "numeric", len = 1L, lower = 0, na.ok = FALSE)
  checkArg(operator.control$mutator.gauss.sd, "numeric", len = 1L, lower = 0, na.ok = FALSE)
}
