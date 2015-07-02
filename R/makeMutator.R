#' @title 
#'   Construct a mutation operator
#' @description
#'   Helper function which constructs a mutator, i.e., a mutation operator.
#'   
#' @param mutator [\code{function}]\cr
#'   Actual mutation operator.
#' @param name [\code{character(1)}]\cr
#'   Name of the mutator.
#' @param description [\code{character(1)}]\cr
#'   Short description of how the mutator works.
#' @param supported [\code{character}]\cr
#'   Vector of strings/names of supported parameter representations. Possible are
#'   'permutation', 'binary', 'float', 'custom'.
#' @param defaults [\code{list}]\cr
#'   List of default values for the operators strategy parameters.
#' @param checker [\code{function}]\cr
#'   Check object, which performs a sanity check in mutator strategy parameters
#'   passed to the control object.
#' @return [\code{ecr_mutator}]
#'   Mutator object.
#' @export
makeMutator = function(mutator, name, description
                       , supported = getAvailableRepresentations()
                       , defaults = list()
                       , checker = function(operator.control) TRUE
                       ) {
  # create operator
  mutator = makeOperator(mutator, name, description, supported, defaults, checker)
  mutator = addClasses(mutator, c("ecr_mutator"))
  return(mutator)
}

# Helper function which returns all supported parameter representations.
getAvailableRepresentations = function() {
  c("permutation", "binary", "float", "custom")
}
