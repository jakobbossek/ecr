#' @title
#'   The nullRecombinator does not perform any recombination.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeNullRecombinator = function() {
  recombinator = function(inds, control=list()) {
    return(inds[[1L]])
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "NULL recombinator",
    description = "Does not perform any recombination.",
    supported = getAvailableRepresentations(),
    n.parents = 10L
  )
}
