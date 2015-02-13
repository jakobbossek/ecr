#' Generator of the crossover recombination operator.
#'
#' @param setOfIndividuals [\code{setOfIndividuals}]\cr
#'   Set of individuals.
#' @param control [\code{list}]\cr
#'   Empty list. Intermediate recombinator has no parameters.
#' @return [\code{setOfIndividuals}]
#'   Recombined offspring.
#' @export
crossoverRecombinator = function(setOfIndividuals, control = list()) {
    parents = setOfIndividuals$individuals
    parent1 = parents[1, ]
    parent2 = parents[2, ]
    n = length(parent1)
    # at least one allele of each parent should be contained
    idx = sample(2:(n - 1), size = 1L)
    child = parent1
    child[idx:n] = parent2[idx:n]
    child = matrix(child, nrow = 1L)
    makePopulation(child)
}

attr(crossoverRecombinator, "name") = "Crossover recombinator"
attr(crossoverRecombinator, "description") = "No description"
attr(crossoverRecombinator, "supported") = c("float", "binary")
attr(crossoverRecombinator, "n.parents") = 2L
attr(crossoverRecombinator, "defaults") = list()
attr(crossoverRecombinator, "class") = c("ecr_operator", "ecr_recombinator")

crossoverRecombinatorCheck = function(operator.control) {}

attr(crossoverRecombinator, "checkFunction") = crossoverRecombinatorCheck
