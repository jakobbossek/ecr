#' @title
#'   Creates offspring from a given mating pool of parents.
#'
#' @param matingPool [\code{setOfIndividuals}]\cr
#'   Set of parents to choose from.
#' @param task [\code{ecr_optimization_tasl}]\cr
#'   Optimization task.
#' @param STORAGE [\code{list}]\cr
#'   List which contains all the algorithm specific stuff.
#' @param objective.fun [\code{function}]\cr
#'   Target fun.
#' @param control [\code{ecr_control}]\cr
#'   Control object containing all operators and further parameters.
#' @param opt.path [\code{OptPath}]\cr
#'   Optimization path.
#' @return [\code{setOfIndividuals}] Generated offspring.
generateOffspring = function(matingPool, task, STORAGE, objective.fun, control, opt.path) {
  n.offspring = control$n.offspring
  offspring = vector(mode = "list", length = n.offspring)

  i.offspring = 1L
  while(i.offspring <= n.offspring) {
    parents = getParents(matingPool, n.parents = getNumberOfParentsNeededForMating(control))
    children = recombine(parents, task, control)
    # eventually the recombinator returns multiple children
    if (hasAttributes(children, "multiple")) {
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

  offspring.fitness = computeFitness(makePopulation(offspring), objective.fun, task, control)

  return(makePopulation(offspring, offspring.fitness))
}

#' @title
#'   Helper method to extract two parents from the mating pool
#'
#' @param matingPool [ecr_population]
#'   Set of individuals selected for reproduction.
#' @param n.parents [integer(1)]
#'   Number of individuals to select.
#' @return [list]
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
