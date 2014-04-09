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
  checkArg(name, cl = "character", len = 1L, na.ok = FALSE)
  checkArg(supported, subset = getAvailableRepresentations(), na.ok = FALSE)

  #FIXME: there is no checkArg option in BBmisc to check if argument is a function!
  if (!is.function(mutator)) {
    stopf("Mutator must be a function.")
  }
  attr(mutator, "name") = name
  attr(mutator, "supported") = supported
  #FIXME: in BBmisc addClasses should have an option not to add already existent classes
  mutator = addClasses(mutator, c("esoo_mutator", "esoo_operator"))
  return(mutator)
}

# Helper function which returns all supported parameter representations.
getAvailableRepresentations = function() {
  c("permutation", "binary", "float", "integer")
}
