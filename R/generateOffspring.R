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
#'   Control object containing alle the operators and further parameters.
#' @param opt.path [\code{OptPath}]\cr
#'   Optimization path.
#' @return [\code{setOfIndividuals}] Generated offspring.
generateOffspring = function(matingPool, task, STORAGE, objective.fun, control, opt.path) {
  n.offspring = control$n.offspring
  offspring = vector(mode = "list", length = n.offspring)

  i = 1L
  while(i <= n.offspring) {
    parents = getParents(matingPool)
    children = recombine(control, parents, task)
    # eventually the recombinator returns multiple children
    if (hasAttributes(children, "multiple")) {
      max.children = min(length(children), n.offspring - i + 1L)
      for (j in seq(max.children)) {
        offspring[[i]] = mutate(control, children[[j]], task)
        i = i + 1L
      }
    } else {
      offspring[[i]] = mutate(control, children, task)
      i = i + 1L
    }
  }

  offspring.fitness = computeFitness(makePopulation(offspring), objective.fun)

  return(makePopulation(offspring, offspring.fitness))
}

#' @title
#'   Helper method to extract two parents from the mating pool
#'
#' @param matingPool [ecr_population]
#'   Set of individuals selected for reproduction.
#' @return [list]
getParents = function(matingPool) {
  inds = matingPool$individuals
  n = length(inds)
  # if we have only one individual, return it twice
  if (n == 1L) {
    return(inds[c(1, 1)])
  }
  idx = sample(n, size = 2L, replace = FALSE)
  return(inds[idx])
}
