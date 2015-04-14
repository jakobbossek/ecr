# Helper function which defines a selector method, i. e., an operator which
# takes the population return a part of it for mating.
#
# @param selector [\code{function}]\cr
#   Actual selection operator.
# @param name [\code{character(1)}]\cr
#   Name of the selector.
# @param description [\code{character(1)}]\cr
#   Short description of how the selector works.
# @param supported [\code{character}]\cr
#   Vector of strings/names of supported parameter representations. For example
#   'permutation', 'float', 'binary'.
# @return [\code{ecr_selector}]
#   selector object.
makeSelector = function(selector, name, description,
  supported = getAvailableRepresentations()) {
  assertFunction(selector, args = c("population", "n.mating.pool"), ordered = TRUE)
  selector = makeOperator(selector, name, description, supported)
  selector = addClasses(selector, c("ecr_selector"))
  return(selector)
}
