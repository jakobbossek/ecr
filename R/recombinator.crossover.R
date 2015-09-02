#' @title
#'   Generator of the One-point crossover recombination operator.
#'
#' @param recombinator.crossover.prob [\code{numeric(1)}]\cr
#'   Cross over probability to form an offspring. Default is \code{1}.
#' @return [\code{ecr_recombinator}]
#' @export
makeCrossoverRecombinator = function(recombinator.crossover.prob = 1) {
  recombinatorCheck = function(operator.control) {
    assertNumber(operator.control$recombinator.crossover.prob, lower = 0, upper = 1)
  }
  
  force(recombinator.crossover.prob)
  defaults = list(recombinator.crossover.prob = recombinator.crossover.prob)
  recombinatorCheck(defaults)
  
  recombinator = function(inds, args = defaults, control = list(), task) {
    n.params = getParamLengths(control$par.set)
    # recombinator only works with individuals which are a vector.
    # Lists of vectors are not handled in this function
    if (length(n.params) > 1L) {
      stopf("This recombinator works with vectors only.")
    }
    # we have to make sure, that the gene has length > 1. This should not
    # be the case in pratice use, but it causes errors
    if (n.params <= 1L) {
      stopf("Crossover recombinator requires genes to have length > 1.")
    }
    
    # get parents
    parent1 = inds[[1]]
    parent2 = inds[[2]]

    # do a cross-over or not
    if (runif(1L) >= args$recombinator.crossover.prob) {
      return(wrapChildren(parent1, parent2))
    }
    
    idx = sample(seq(n.params - 1), size = 1L)
    # at least one allele of each parent should be contained
    # back part from other parent
    child1 = parent2
    child2 = parent1
    # front part from "their" parent
    child1[1:idx] = parent1[1:idx]
    child2[1:idx] = parent2[1:idx]
    
    return(wrapChildren(child1, child2))
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Crossover recombinator",
    description = "Performs classical one-point crossover.",
    n.parents = 2L,
    supported = c("float", "binary"),
    defaults = defaults,
    checker = recombinatorCheck
  )
}
