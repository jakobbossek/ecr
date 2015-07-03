#' The nullRecombinator does not perform any recombination.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeNullRecombinator = function() {
  recombinator = function(inds, args = list(), control=list()) {
    if (length(inds) > 1)
      attr(inds, "children") = TRUE
    
    return(inds)
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "NULL recombinator",
    description = "Does not perform any recombination.",
    supported = getAvailableRepresentations(),
    n.parents = 10L
  )
}
