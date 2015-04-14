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
generateOffspring = function(matingPool, objective.fun, control, opt.path) {
  generator = control$generator
  mutator = control$mutator
  mutationStrategyAdaptor = control$mutationStrategyAdaptor
  mutator.control = control$mutator.control
  recombinator = control$recombinator
  #parentSelector = control$selector
  parentSelector = simpleUniformSelection
  n.offspring = control$n.offspring

  #offspring = list()
  offspring = vector("list", n.offspring)

  for (i in 1:n.offspring) {
    parents = parentSelector(matingPool)
    # pass just the individuals and get a single individual
    child = recombinator(parents$individuals)
    mutator.control = mutationStrategyAdaptor(mutator.control, opt.path)
    # pass just the individual and get a single individual
    child = mutator(child, mutator.control)
    offspring[[i]] = child
  }
  if (control$representation != "custom") {
    offspring = correctBounds(offspring, par.set = getParamSet(objective.fun))
  }
  offspring.fitness = computeFitness(makePopulation(offspring), objective.fun)

  return(makePopulation(offspring, offspring.fitness))
}

simpleUniformSelection = function(matingPool) {
  population = matingPool$individuals
  fitness = matingPool$fitness
  n = length(population)
  # if we have only one individual, return it twice
  if (n == 1) {
    return(makePopulation(population[c(1, 1)], fitness[c(1, 1)]))
  }
  idx = sample(n, size = 2, replace = FALSE)
  makePopulation(population[idx], fitness[idx])
}
