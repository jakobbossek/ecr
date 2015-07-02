# Helper function which constructs an evolutionary operator.
#
# @param operator [\code{function}]\cr
#   Actual mutation operator.
# @param name [\code{character(1)}]\cr
#   Name of the operator.
# @param description [\code{character(1)}]\cr
#   Short description of how the mutator works.
# @param supported [\code{character}]\cr
#   Vector of names of supported parameter representations. For example
#   'permutation', 'float', 'binary'.
# @param defaults [\code{list}]\cr
#   List of default values for the operators strategy parameters.
# @return [\code{ecr_operator}]
#   Operator object.
makeOperator = function(operator, name, description
                        , supported = getAvailableRepresentations()
                        , defaults = list()
                        , checker = function(operator.control) TRUE
                        ) {
  assertFunction(operator)
  assertString(name)
  assertString(description)
  assertSubset(supported, choices = getAvailableRepresentations(), empty.ok = FALSE)
  assertList(defaults, unique = TRUE, any.missing = FALSE)
  assertFunction(checker, args = "operator.control")

  attr(operator, "name") = name
  attr(operator, "description") = description
  attr(operator, "supported") = supported
  attr(operator, "defaults") = defaults
  attr(operator, "checker") = checker

  operator = addClasses(operator, c("ecr_operator"))
  return(operator)
}

#' Checks if given function is an ecr operator.
#'
#' @param obj [any]\cr
#'   Arbitrary R object to check.
#' @return [\code{logical(1)}]
#' @export
isEcrOperator = function(obj) {
  return(inherits(obj, "ecr_operator"))
}
