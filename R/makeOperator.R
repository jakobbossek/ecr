# Helper function which constructs an evolutionary operator.
#
# @param operator [\code{function}]\cr
#   Actual mutation operator.
# @param name [\code{character(1)}]\cr
#   Name of the operator.
# @param supported [\code{character}]\cr
#   Vector of names of supported parameter representations. For example
#   'permutation', 'float', 'binary'.
# @return [\code{ecr_operator}]
#   Operator object.
makeOperator = function(operator, name, supported = getAvailableRepresentations()) {
  assertFunction(operator)
  assertCharacter(name, len = 1L, any.missing = FALSE)
  assertSubset(supported, choices = getAvailableRepresentations(), empty.ok = FALSE)

  attr(operator, "name") = name
  attr(operator, "supported") = supported

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
