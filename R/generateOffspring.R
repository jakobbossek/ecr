# @title
# Creates offspring from a given mating pool of parents.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Optimization state.
# @param matingPool [\code{ecr_population}]\cr
#   Mating pool to select individuals from.
# @param control [\code{ecr_control}]\cr
#   Control object.
# @return [\code{setOfIndividuals}] Generated offspring.
generateOffspring = function(opt.state, matingPool, control) {
  n.offspring = control$n.offspring
  task = opt.state$task
  fitness.fun = task$fitness.fun
  offspring = vector(mode = "list", length = n.offspring)

  i.offspring = 1L
  while(i.offspring <= n.offspring) {
    # select parents for mating
    parents = getParents(matingPool, n.parents = getNumberOfParentsNeededForMating(control))
    children = recombine(parents, task, control)
    # eventually the recombinator returns multiple children
    if (hasAttributes(children, "multiple")) {
      # maybe we got two children, but we only have place for one left
      max.children = min(length(children), n.offspring - i.offspring + 1L)
      for (j in seq(max.children)) {
        offspring[[i.offspring]] = mutate(children[[j]], task, control)
        i.offspring = i.offspring + 1L
      }
    } else {
      offspring[[i.offspring]] = mutate(children, task, control)
      i.offspring = i.offspring + 1L
    }
  }

  offspring.fitness = evaluateFitness(makePopulation(offspring), fitness.fun, task, control)

  return(makePopulation(offspring, offspring.fitness))
}

# @title
# Helper method to extract two parents from the mating pool
#
# @param matingPool [\code{ecr_population}]
#   Set of individuals selected for reproduction.
# @param n.parents [\code{integer(1)}]
#   Number of individuals to select.
# @return [\code{list}]
getParents = function(matingPool, n.parents = 2L) {
  inds = matingPool$individuals
  n = length(inds)
  # if we have only one individual, return it twice
  if (n == 1L) {
    return(inds[rep(1, n.parents)])
  }
  idx = sample(n, size = n.parents, replace = TRUE)
  return(inds[idx])
}
