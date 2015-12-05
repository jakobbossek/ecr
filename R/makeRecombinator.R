#' @title Construct a recombination operator.
#'
#' @description
#' Helper function which constructs a recombinator, i. e., a recombination operator.
#'
#' @note
#' If a recombinator returns more than one child, the \code{multiple.children}
#' parameter needs to be \code{TRUE}, which is the default. In case of multiple
#' children produced these have to be placed within a list.
#'
#' @param recombinator [\code{function}]\cr
#'   Actual mutation operator.
#' @param name [\code{character(1)}]\cr
#'   Name of the recombinator.
#' @param description [\code{character(1)}]\cr
#'   Short description of how the recombinator works.
#' @param supported [\code{character}]\cr
#'   Vector of strings/names of supported parameter representations. For example
#'   'permutation', 'float', 'binary'.
#' @param params [\code{list}]\cr
#'   Named list of the parameters the operator has been initialized with.
#'   Default is the empty list.
#' @param n.parents [\code{integer(1)}]\cr
#'   Number of parents supported.
#' @param multiple.children [\code{logical(1)}]\cr
#'   Does the recombinator return more than one child? Default is \code{TRUE}.
#' @return [\code{ecr_recombinator}]
#'   Recombinator object.
#' @export
makeRecombinator = function(
  recombinator, name, description,
  supported = getAvailableRepresentations(),
  params = list(),
  n.parents = 2L,
  multiple.children = TRUE) {
  assertFunction(recombinator, args = c("inds", "task", "control"), ordered = TRUE)
  assertInteger(n.parents, len = 1L, lower = 2L, any.missing = FALSE)
  assertFlag(multiple.children)

  recombinator = makeOperator(recombinator, name, description, supported, params)
  attr(recombinator, "n.parents") = n.parents
  attr(recombinator, "multiple.children") = multiple.children

  recombinator = addClasses(recombinator, c("ecr_recombinator"))

  return(recombinator)
}

generatesMultipleChildren = function(recombinator) {
  UseMethod("generatesMultipleChildren")
}

generatesMultipleChildren.ecr_recombinator = function(recombinator) {
  return(attr(recombinator, "multiple.children"))
}

getNumberOfParentsNeededForMating = function(control) {
  return(attr(control$recombinator, "n.parents"))
}
