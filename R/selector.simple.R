#' Simple (naive) mating pool generator.
#'
#' Just for testing. Actually does not really select, but instead returns the
#' entire population to form the mating pool.
#'
#' @return [\code{setOfIndividuals}]
#' @export
makeSimpleSelector = function() {
  selector = function(population, n.mating.pool) {
    # inds = population$individuals
    # fitn = population$fitness
    # n = length(inds)
    # idx = sample(n, size = 2, replace = FALSE)
    # return(makePopulation(inds[idx], fitn[idx]))
    return(population)
  }
  makeSelector(
    selector = selector,
    name = "Simple selector",
    description = "Simply returns the entire population."
  )
}
