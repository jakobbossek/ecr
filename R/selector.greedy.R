#' Simple \dQuote{greedy} selector.
#'
#' Sorts the individuals according to their fitness value in increasing order
#' and selects the best ones.
#'
#' @return [\code{setOfIndividuals}]
#' @export
makeGreedySelector = function() {
  selector = function(population, n.select, control) {
    inds = population$individuals
    fitn = population$fitness
    idx.select = order(fitn)[seq(n.select)]
    return(makePopulation(inds[idx.select], fitn[idx.select]))
  }
  makeSelector(
    selector = selector,
    name = "Greedy selector",
    description = "Return the best individuals regarding the fitness value."
  )
}
