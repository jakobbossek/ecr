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
#FIXME: extract offspring.size form control or better provide as seperate argument?
#       The later would be more intuitive.
#FIXME: maybe also clean up control before giving it to this function? I.e., providing
#       just the operators in control and not all the other overhead.
generateOffspring = function(matingPool, objective.fun, control) {
  generator = control$generator
  mutator = control$mutator
  mutator.control = control$mutator.control
  recombinator = control$recombinator
  #FIXME: until now we only draw randomly (uniformly!) from the mating pool
  #       We need rhoullette-wheel-selection and other, better algorithms.
  #parentSelector = control$parentSelector
  parentSelector = simpleUniformSelection
  offspring.size = control$offspring.size
  n.params = control$n.params

  offspring = list()
  #FIXME: make this better. We can work with the apply family here. But I think
  #       we must give up the 'setOfIndividuals/ecr_population' types for this.
  #       Moreover this might be really helpful because all the wrapping and un-
  #       wrapping sucks hard!
  for (i in 1:offspring.size) {
    parents = parentSelector(matingPool)
    child = recombinator(parents)
    child = mutator(child, mutator.control)
    child = computeFitness(child, objective.fun)
    #FIXME: what about all the post-processing funs?
    #child = correctBounds(child, lower, upper)
    offspring[[i]] = child
  }
  #FIXME: this works only for offspring.size = 2L. Otherwise it craches!
  offspring = do.call(mergePopulations, offspring)

  return(offspring)
}

simpleUniformSelection = function(matingPool) {
  population = matingPool$population
  fitness = matingPool$fitness
  n = nrow(population)
  idx = sample(n, size = 2, replace = FALSE)
  makePopulation(population[idx, , drop = FALSE], fitness[idx])
}
