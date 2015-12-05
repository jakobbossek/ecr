#' @title
#'   This recombinator does not perform any recombination.
#'
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
makeNullRecombinator = function() {
  recombinator = function(inds, task, control) {
    return(inds[[1L]])
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "NULL recombinator",
    description = "Does not perform any recombination.",
    supported = getAvailableRepresentations(),
    n.parents = 10L,
    multiple.children = FALSE
  )
}
