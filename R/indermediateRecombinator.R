#' Generator of the indermediate recombination operator.
#'
#' @param individuals [\code{setOfIndividuals}]\cr
#'   Set of individuals.
#' @param control [\code{list}]\cr
#'   Empty list. Intermediate recombinator has no paramerters.
#' @return [\code{setOfIndividuals}]
#'   Recombined offspring.
#' @export
intermediateRecombinator = function(setOfIndividuals, control=list()) {
  child = apply(setOfIndividuals$population, 2, sum) / 2
  makePopulation(t(as.matrix(child)))
}

attr(intermediateRecombinator, "name") = "Intermediate recombinator"
attr(intermediateRecombinator, "description") = "No description"
attr(intermediateRecombinator, "supported") = c("float")
attr(intermediateRecombinator, "n.parents") = 10L
attr(intermediateRecombinator, "defaults") = list()
attr(intermediateRecombinator, "class") = c("ecr_operator", "ecr_recombinator")

intermediateRecombinatorCheck = function(operator.control) {}
