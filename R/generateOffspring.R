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
  recombinator.control = control$recombinator.control
  n.offspring = control$n.offspring

  offspring = vector("list", n.offspring)

  i.offspring = 1
  while (i.offspring <= n.offspring) {
    #catf("Parent %i", i.offspring)
    parents = getParents(matingPool)
    #print(parents)
    # pass just the individuals and get a child or children
    # children have the attribute "children" = TRUE
    child = recombinator(parents, recombinator.control, control)
    #catf("Child %i", i.offspring)
    #print(child)
    mutator.control = mutationStrategyAdaptor(mutator.control, opt.path)
    # mutate the child or children
    if (isTRUE(attr(child, "children"))) {
      max.children = min(length(child), n.offspring - i.offspring + 1)
      for (i.child in seq(max.children)) {
        # pass just the individual and get a single individual
        child[[i.child]] = mutator(child[[i.child]], mutator.control, control)
        offspring[[i.offspring]] = child[[i.child]]
        i.offspring = i.offspring + 1
      }
    } else {
      # pass just the individual and get a single individual
      child = mutator(child, mutator.control, control)
      offspring[[i.offspring]] = child
      i.offspring = i.offspring + 1
    }
  }
  #print(offspring)
  offspring.fitness = computeFitness(makePopulation(offspring), objective.fun, control)

  #print(makePopulation(offspring, offspring.fitness))
  #stop()
  return(makePopulation(offspring, offspring.fitness))
}

# Helper method to extract two parents from the mating pool
#
# @param matingPool [ecr_population]
#   Set of individuals selected for reproduction.
# @return [list]
#FIXME: generalize to more than two parents
getParents = function(matingPool) {
  inds = matingPool$individuals
  n = length(inds)
  # if we have only one individual, return it twice
  if (n == 1) {
    return(inds[c(1, 1)])
  }
  idx = sample(n, size = 2, replace = FALSE)
  return(inds[idx])
}
