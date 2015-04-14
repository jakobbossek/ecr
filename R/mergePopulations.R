# Helper for merging populations.
#
# @param ... [\code{list}]\cr
#  List of objects of type \code{setOfIndividuals}.
# @return [\code{setOfIndividuals}]
mergePopulations = function(...) {
  populations = list(...)
  #stop()

  # get n.params
  n.params = length(populations[[1]]$individuals[[1]])

  # summarize over all population sizes
  pop.size = sum(sapply(populations, function(x) length(x$fitness)))

  # allocate space
  fitness = numeric()
  individuals = list()

  # now iterate over populations and generate merged population
  start = 1L
  for (i in 1:length(populations)) {
    j = length(populations[[i]]$individuals[[1]])
    individuals = c(individuals, populations[[i]]$individuals)
    fitness = c(fitness, unlist(populations[[i]]$fitness))
    start = start + j
  }
  makePopulation(
    individuals = individuals,
    fitness = fitness
  )
}
