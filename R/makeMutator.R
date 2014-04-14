# Helper function which constructs a mutator, i. e., a mutation operator.
#
# @param mutator [\code{function}]\cr
#   Actual mutation operator.
# @param name [\code{character(1)}]\cr
#   Name of the mutator.
# @param supported [\code{character}]\cr
#   Vector of strings/names of supported parameter representations. For example
#   'permutation', 'float', 'binary'.
# @return [\code{esoo_mutator}]
#   Mutator object.
makeMutator = function(mutator, name, supported = getAvailableRepresentations()) {
  mutator = makeOperator(mutator, name, supported)
  mutator = addClasses(mutator, c("esoo_mutator"))
  return(mutator)
}

# Helper function which returns all supported parameter representations.
getAvailableRepresentations = function() {
  c("permutation", "binary", "float", "integer")
}
