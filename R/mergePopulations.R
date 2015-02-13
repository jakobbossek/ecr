# Helper for merging populations.
#
# @param ... [\code{list}]\cr
#  List of objects of type \code{setOfIndividuals}.
# @return [\code{setOfIndividuals}]
mergePopulations = function(...) {
  populations = list(...)
  # get n.params
  n.params = ncol(populations[[1]]$individuals)

  # summarize over all population sizes
  pop.size = sum(sapply(populations, function(x) length(x$fitness)))

  # allocate space
  fitness = numeric(pop.size)
  individuals = matrix(NA, ncol = n.params, nrow = pop.size)

  # now iterate over populations and generate merged population
  #FIXME: maybe better implement this in C++
  start = 1L
  for (i in 1:length(populations)) {
    j = nrow(populations[[i]]$individuals)
    individuals[start:(start + j - 1), ] = populations[[i]]$individuals
    fitness[start:(start + j - 1)] = populations[[i]]$fitness
    start = start + j
  }
  makePopulation(
    individuals = individuals,
    fitness = fitness
  )
}
