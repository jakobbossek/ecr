#' @title Construct a selection operator.
#'
#' @description
#' Helper function which defines a selector method, i. e., an operator which
#' takes the population and returns a part of it for mating or survival.
#'
#' @param selector [\code{function}]\cr
#'   Actual selection operator.
#' @param name [\code{character(1)}]\cr
#'   Name of the selector.
#' @param description [\code{character(1)}]\cr
#'   Short description of how the selector works.
#' @param supported [\code{character}]\cr
#'   Vector of strings/names of supported parameter representations. For example
#'   'permutation', 'float', 'binary'.
#' @param supported.objectives [\code{character}]\cr
#'   At least one of \dQuote{single-objective} or \dQuote{multi-objective}.
#' @param supported.opt.directions [\code{character(1-2)}]\cr
#'   Does the selector work for maximization tasks xor minimization tasks or both?
#'   Default is \code{c("maximze", "minimize")}, which means both optimization
#'   \dQuote{directions} are supported.
#' @return [\code{ecr_selector}]
#'   Selector object.
#' @export
makeSelector = function(
  selector,
  name, description,
  supported = getAvailableRepresentations(),
  supported.objectives,
  supported.opt.directions = c("minimize", "maximze")) {
  assertFunction(selector, args = c("population", "storage", "task", "n.select", "control"), ordered = TRUE)
  assertSubset(supported.objectives, c("single-objective", "multi-objective"))
  assertSubset(supported.opt.directions, c("maximze", "minimize"))
  selector = makeOperator(selector, name, description, supported)
  selector = setAttribute(selector, "supported.objectives", supported.objectives)
  selector = setAttribute(selector, "supported.opt.directions", supported.opt.directions)
  selector = addClasses(selector, c("ecr_selector"))
  return(selector)
}
