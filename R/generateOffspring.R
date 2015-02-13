# Creates offspring from a given mating pool of parents.
#
# @param matingPool [\code{setOfIndividuals}]\cr
#   Set of parents to choose from.
# @param objective.fun [\code{function}]\cr
#   Target fun.
# @param control [\code{ecr_control}]\cr
#   Control object containing alle the operators and further parameters.
# @return [\code{setOfIndividuals}]
#   Generated offspring.
generateOffspring = function(matingPool, objective.fun, control) {
  generator = control$generator
  mutator = control$mutator
  mutator.control = control$mutator.control
  recombinator = control$recombinator
  #parentSelector = control$parentSelector
  parentSelector = simpleUniformSelection
  offspring.size = control$offspring.size
  n.params = control$n.params

  #offspring = list()
  offspring = matrix(NA, ncol = n.params, nrow = offspring.size)

  for (i in 1:offspring.size) {
    parents = parentSelector(matingPool)
    child = recombinator(parents)
    for (j in 1:control$n.mutators) {
      mutator.fun = mutator[[j]]
      mutator.params = mutator.control[[j]]
      child = mutator.fun(child, mutator.params)
      # catf("Applying mutator %i of %i", j, control$n.mutators)
    }
    offspring[i, ] = child$individuals
  }
  offspring.fitness = computeFitness(makePopulation(offspring), objective.fun)

  return(makePopulation(offspring, offspring.fitness))
}

simpleUniformSelection = function(matingPool) {
  population = matingPool$individuals
  fitness = matingPool$fitness
  n = nrow(population)
    idx = sample(n, size = 2, replace = FALSE)
  makePopulation(population[idx, , drop = FALSE], fitness[idx])
}
