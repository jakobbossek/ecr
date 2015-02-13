#' Extracts the best individual of a set of individuals.
#'
#' @param population [\code{setOfIndividuals}]\cr
#'  Population.
#' @return [\code{essoIndividual}]
#'  Individual with best, i. e., lowest fitness value.
#' @export
getBestIndividual = function(population) {
    fitness = population$fitness
    best.idx = which.min(population$fitness)
    best.fitness = fitness[best.idx]
    best.individual = population$individuals[best.idx, ]
    return(list(individual = best.individual, fitness = best.fitness))
}
