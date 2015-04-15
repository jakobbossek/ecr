#' Generator for Roulette-Wheel-Selection (fitness-proportional selection).
#'
#' Selects genes for the mating pool. The chance of an individual to get selected
#' is proportional to its fitness, i.e., better individuals get a higher chance
#' to take part in the reproduction process. Low-fitness individuals however,
#' have a positive fitness as well.
#'
# FIMXE: add thorough description of the heuristic used (see below).
#' @return [\code{setOfIndividuals}]
#' @export
makeRouletteWheelSelector = function() {
  selector = function(population, n.mating.pool) {
    #FIXME: We do minimization; Roulette-Wheel selection cannot be used in
    # minimization in general, since low fitness values lead to low selection
    # probabilities. Another problem are negative fitness values.
    # Here we do the following: a) "shift to positive" by adding the minimal fitness
    # value to the fitness vector and add the magic number m = 0.1, i.e., (-10, -5, 20)
    # becomes (0.1, 5.1, 20) to avoid zero fitness. b) Proceed to the reciprocal
    # (10, 1 / 5.1, 1 / 20)
    # But how to choose the magic number and is this really ok?
    inds = population$individuals
    fitness = population$fitness
    if (any(fitness <= 0L)) {
      fitness = fitness + abs(min(fitness)) + 0.1
    }
    fitness = 1 / fitness
    n.population = length(inds)
    prob = fitness / sum(fitness)
    idx = sample(n.population, size = n.mating.pool, replace = TRUE, prob = prob)
    return(makePopulation(inds[idx], population$fitness[idx]))
  }
  makeSelector(
    selector = selector,
    name = "Roulette-Wheel selector",
    description = "Selects individuals in a fitness-proportional fashion."
  )
}
