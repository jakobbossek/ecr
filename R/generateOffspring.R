#' @title
#' Creates offspring from a given mating pool of parents.
#'
#' @description
#' Given and optimization state and a mating pool of individuals this function
#' generates offspring individuals based on the parameters specified in the
#' control object.
#'
#' @param opt.state [\code{ecr_opt_state}]\cr
#'   Optimization state.
#' @param mating.pool [\code{ecr_population}]\cr
#'   Mating pool to select individuals from.
#' @param control [\code{ecr_control}]\cr
#'   Control object.
#' @return [\code{ecr_population}] Generated offspring.
#' @export
generateOffspring = function(opt.state, mating.pool, control) {
  n.offspring = control$n.offspring
  task = opt.state$task
  fitness.fun = task$fitness.fun
  offspring = vector(mode = "list", length = n.offspring)
  recombinator = control$recombinator

  n.children = getNumberOfChildren(recombinator)
  n.recomb.calls = ceiling(as.numeric(n.offspring) / n.children)

  # actually generate offspring
  offspring = parallelMap(function(i) {
    parents = getParents(mating.pool, n.parents = getNumberOfParentsNeededForMating(recombinator))
    children = recombine(parents, task, control)
    off = if (hasAttributes(children, "multiple")) children else list(children)
    off = lapply(off, function(x) {
      mutate(x, task, control)
    })
    return(off)
  }, seq(n.recomb.calls), level = "ecr.generateOffspring")

  # we need to "unwrap" one listing layer here
  offspring = unlist(offspring, recursive = FALSE)

  # if n.children is odd/even and n.offspring is even/odd we need to remove one child
  if (length(offspring) > n.offspring) {
    offspring = offspring[-sample(1:length(offspring), 1L)]
  }

  offspring.fitness = evaluateFitness(makePopulation(offspring), fitness.fun, task, control)
  return(makePopulation(offspring, offspring.fitness))
}

# @title
# Helper method to extract two parents from the mating pool
#
# @param mating.pool [\code{ecr_population}]
#   Set of individuals selected for reproduction.
# @param n.parents [\code{integer(1)}]
#   Number of individuals to select.
# @return [\code{list}]
getParents = function(mating.pool, n.parents = 2L) {
  inds = mating.pool$individuals
  n = length(inds)
  # if we have only one individual, return it twice
  if (n == 1L) {
    return(inds[rep(1, n.parents)])
  }
  idx = sample(n, size = n.parents, replace = TRUE)
  return(inds[idx])
}
