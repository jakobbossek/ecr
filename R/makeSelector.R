#' @title
#'   Construct a selection operator.
#'
#' @description
#'   Helper function which defines a selector method, i. e., an operator which
#'   takes the population and returns a part of it for mating.
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
#' @return [\code{ecr_selector}]
#'   selector object.
#' @export
makeSelector = function(
  selector,
  name, description,
  supported = getAvailableRepresentations(),
  supported.objectives) {
  assertFunction(selector, args = c("population", "storage", "task", "n.select", "control"), ordered = TRUE)
  assertSubset(supported.objectives, c("single-objective", "multi-objective"))
  selector = makeOperator(selector, name, description, supported)
  selector = setAttribute(selector, "supported.objectives", supported.objectives)
  selector = addClasses(selector, c("ecr_selector"))
  return(selector)
}
