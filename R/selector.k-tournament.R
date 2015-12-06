#' @title
#' Generator for k-Tournament selection.
#'
#' @description
#' Selects genes for the mating pool. It works as follows: k individuals from the
#' population are chosen randomly and the best one is selected to be included into
#' the mating pool. This process is repeated until the desired number of individuals
#' for the mating pool is reached.
#'
#' @param k [\code{integer(1)}]\cr
#'   Number of individuals to participate in each tournament. Default is \code{2L}.
#' @return [\code{setOfIndividuals}]
#' @family selectors
#' @export
makeTournamentSelector = function(k = 3L) {
  assertInt(k, na.ok = FALSE, lower = 2L)

  force(k)

  selector = function(population, storage, task, n.select, control) {
    inds = population$individuals
    fitness = as.numeric(population$fitness)
    n.population = length(fitness)
    pop.idx = seq(n.population)

    idx = integer(n.select)
    for (i in seq(n.select)) {
      # choose k individuals at random ...
      competitor.idx = sample(pop.idx, size = k)
      # ... and store the best
      idx[i] = competitor.idx[which.min(fitness[competitor.idx])]
    }

    return(makePopulation(inds[idx], matrix(fitness[idx], nrow = 1L)))
  }
  makeSelector(
    selector = selector,
    name = "k-Tournament selector",
    description = "Select k individuals at random, choose the best, repeat until mating pool is filled.",
    supported.objectives = c("single-objective")
  )
}
