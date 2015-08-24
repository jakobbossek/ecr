#' @title
#'   Construct a recombination operator.
#' @description
#'   Helper function which constructs a recombinator, i. e., a recombination operator.
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
#' @param n.parents [\code{integer(1)}]\cr
#'   Number of parents supported.
#' @param defaults [\code{list}]\cr
#'   List of default strategy parameter values.
#' @param checker [\code{function}]\cr
#'   Check object, which performs a sanity check in mutator strategy parameters
#'   passed to the control object.
#' @return [\code{ecr_recombinator}]
#'   Recombinator object.
#' @export
makeRecombinator = function(
  recombinator, name, description,
  supported = getAvailableRepresentations(),
  n.parents = 2L,
  defaults = list(),
  checker = function(operator.control) TRUE) {
  recombinator = makeOperator(recombinator, name, description, supported, defaults)

  assertInteger(n.parents, len = 1L, lower = 2L, any.missing = FALSE)
  attr(recombinator, "n.parents") = n.parents

  recombinator = addClasses(recombinator, c("ecr_recombinator"))

  return(recombinator)
}
