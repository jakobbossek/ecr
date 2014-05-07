#' The nullRecombinator does not perform any recombination.
#'
#' @param setOfIndividuals [\code{setOfIndividuals}]\cr
#'   Set of individuals.
#' @param control [\code{list}]\cr
#'   Empty list. Intermediate recombinator has no paramerters.
#' @return [\code{setOfIndividuals}]
#'   Recombined offspring.
#' @export
nullRecombinator = function(setOfIndividuals, control=list()) {
  child = setOfIndividuals$population[1, ]
  makePopulation(t(as.matrix(child)))
}

attr(nullRecombinator, "name") = "null recombinator"
attr(nullRecombinator, "description") = "No description"
attr(nullRecombinator, "supported") = getAvailableRepresentations()
attr(nullRecombinator, "n.parents") = 10L
attr(nullRecombinator, "defaults") = list()
attr(nullRecombinator, "class") = c("ecr_operator", "ecr_recombinator")

nullRecombinatorCheck = function(operator.control) {}

attr(nullRecombinator, "checkFunction") = nullRecombinatorCheck
