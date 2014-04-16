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
  checkArg(name, cl = "character", len = 1L, na.ok = FALSE)
  checkArg(supported, subset = getAvailableRepresentations(), na.ok = FALSE)

  #FIXME: there is no checkArg option in BBmisc to check if argument is a function!
  if (!is.function(operator)) {
    stopf("operator must be a function.")
  }
  attr(operator, "name") = name
  attr(operator, "supported") = supported

  #FIXME: in BBmisc addClasses should have an option not to add already existent classes
  operator = addClasses(operator, c("ecr_operator"))
  return(operator)
}
