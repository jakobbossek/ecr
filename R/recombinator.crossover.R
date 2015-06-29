#' Generator of the One-point crossover recombination operator.
#'
#' @param recombinator.crossover.prob [\code{numeric(1)}]\cr
#'   Cross over probability to form an offspring. Default is \code{1}.
#' @return [\code{ecr_recombinator}]
#' @export
makeCrossoverRecombinator = function(recombinator.crossover.prob = 1) {
  recombinatorCheck = function(operator.control) {
    assertNumber(operator.control$recombinator.crossover.prob
                 , lower = 0, upper = 1, na.ok = FALSE
    )
  }
  
  force(recombinator.crossover.prob)
  defaults = list(recombinator.crossover.prob = recombinator.crossover.prob)
  recombinatorCheck(defaults)
  
  recombinator = function(inds, args = defaults, control = list()) {
    parent1 = inds[[1]]
    parent2 = inds[[2]]
    n = length(parent1)
    #FIXME: we have to make sure, that the gene has length > 1. This should not
    # be the case in pratice use, but it causes errors
    if (n == 1L) {
      stopf("Crossover recombinator requires genes to have length > 1.")
    }
    # at least one allele of each parent should be contained
    child1 = parent1
    child2 = parent2
    do.recombinate = runif(1L) < args$recombinator.crossover.prob
    if (do.recombinate) {
      idx = sample(1:(n - 1), size = 1L)
      child1[(idx + 1L):n] = parent2[(idx + 1L):n]
      child2[1:idx] = parent1[1:idx]
    }
    # return two offsprings
    children = list(child1, child2)
    attr(children, "children") = TRUE
    return(children)
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
