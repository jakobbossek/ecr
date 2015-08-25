#' @title
#'   Creates offspring from a given mating pool of parents.
#'
#' @param matingPool [\code{setOfIndividuals}]\cr
#'   Set of parents to choose from.
#' @param STORAGE [\code{list}]\cr
#'   List which contains all the algorithm specific stuff.
#' @param objective.fun [\code{function}]\cr
#'   Target fun.
#' @param control [\code{ecr_control}]\cr
#'   Control object containing alle the operators and further parameters.
#' @param opt.path [\code{OptPath}]\cr
#'   Optimization path.
#' @return [\code{setOfIndividuals}] Generated offspring.
generateOffspring = function(matingPool, STORAGE, objective.fun, control, opt.path) {
  mutator = control$mutator
  recombinator = control$recombinator
  n.offspring = control$n.offspring

  offspring = lapply(seq(n.offspring), function(idx) {
    parents = getParents(matingPool)
    child = recombine(control, parents)
    child = mutate(control, child)
    return(child)
  })
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
