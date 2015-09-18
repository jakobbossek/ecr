#' @title
#'   Generator of the indermediate recombination operator.
#'
#' @param k [integer(1)]\cr
#'   Number of parents required for mating. Default is \code{2}.
#' @return [\code{ecr_recombinator}]
#' @export
makeIntermediateRecombinator = function(k = 2L) {
  assertInt(k, na.ok = FALSE, lower = 2L)

  defaults = list(k = k)
  recombinator = function(inds, control = defaults, task) {
    n = length(inds[[1]])
    child = rep(0, n)
    for (i in 1:length(inds)) {
      child = child + inds[[i]]
    }
    return(child / length(inds))
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Intermediate recombinator",
    description = "Performs intermediate recombination.",
    supported = "float",
    n.parents = k,
    multiple.children = FALSE
  )
}
