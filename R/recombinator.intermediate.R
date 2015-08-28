#' @title
#'   Generator of the indermediate recombination operator.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeIntermediateRecombinator = function() {
  recombinator = function(inds, control = list(), task) {
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
    n.parents = 10L,
    multiple.children = FALSE
  )
}
