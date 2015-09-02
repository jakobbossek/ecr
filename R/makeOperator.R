#' @title
#'   Construct evolutionary operator.
#'
#' @description
#'   Helper function which constructs an evolutionary operator.
#'
#' @note
#'   In general you will not need this function, but rather one of its
#'   deriviatives like \code{\link{makeMutator}} or \code{\link{makeSelector}}.
#'
#' @param operator [\code{function}]\cr
#'   Actual mutation operator.
#' @param name [\code{character(1)}]\cr
#'   Name of the operator.
#' @param description [\code{character(1)}]\cr
#'   Short description of how the mutator works.
#' @param supported [\code{character}]\cr
#'   Vector of names of supported parameter representations. For example
#'   'permutation', 'float', 'binary'.
#' @param defaults [\code{list}]\cr
#'   List of default values for the operators strategy parameters.
#' @param checker [\code{function}]\cr
#'   Check object, which performs a sanity check of the strategy parameters
#'   passed to the control object.
#'   Operator object.
#' @return [\code{ecr_operator}]
#' @export
makeOperator = function(operator, name, description,
  supported = getAvailableRepresentations(),
  defaults = list(),
  checker = function(operator.control) TRUE) {
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

#' @title
#'   Determine the name of a given operator.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Operator object.
#' @return [\code{character(1)}]
#'   Name of the operator.
#' @export
getOperatorName = function(operator) {
  UseMethod("getOperatorName")
}

#' @export
getOperatorName.ecr_operator = function(operator) {
  attr(operator, "name")
}

#' @title
#'   Check if given function is an ecr operator.
#'
#' @param obj [any]\cr
#'   Arbitrary R object to check.
#' @return [\code{logical(1)}]
#' @export
isEcrOperator = function(obj) {
  return(inherits(obj, "ecr_operator"))
}

#' @export
print.ecr_operator = function(x, ...) {
  catf("Name: %s", getOperatorName(x))
  catf("Description: %s", attr(x, "description"))
  catf("Supported representations: %s", collapse(getSupportedRepresentations(x)))
  catf("Default parameters: %s", getParametersAsString(getOperatorDefaultParameters(x)))
}

#' @export
print.ecr_recombinator = function(x, ...) {
  print.ecr_operator(x)
  catf("Number of returned children: %i", attr(x, "n.parents"))
}

#' @export
print.ecr_selector = function(x, ...) {
  print.ecr_operator(x)
  catf("Supported #objectives: %s", attr(x, "supported.objectives"))
}

#' @title
#'   Returns a list with the default parameter values for a given operator.
#'
#' @description
#'   Operators can depend on specific evolutionary parameters. If you do not provide
#'   values for these, specifc defaults are used, which can be determined with this
#'   function.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Operator object.
#' @return [\code{list}]
#'   Key-value pairs of parameters and default values.
#' @export
getOperatorDefaultParameters = function(operator) {
  UseMethod("getOperatorDefaultParameters")
}

#' @export
getOperatorDefaultParameters.ecr_operator = function(operator) {
  if (hasAttributes(operator, "defaults")) {
    return(attr(operator, "defaults"))
  }
  return("")
}

#' @title
#'   Returns the parameter check function of a given operator.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Operator object.
#' @return [\code{character(1)}]
#'   Function for parameter check.
#' @export
getOperatorCheckFunction = function(operator) {
  UseMethod("getOperatorCheckFunction")
}

#' @export
getOperatorCheckFunction.ecr_operator = function(operator) {
  attr(operator, "checker")
}

#' @title
#'   Returns the character vector of tags which describe a specific operator.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Operator object.
#' @return [\code{character}]
#'   Vector of representation types.
#' @export
getSupportedRepresentations = function(operator) {
  UseMethod("getSupportedRepresentations")
}

#' @export
getSupportedRepresentations.ecr_operator = function(operator) {
  attr(operator, "supported")
}

#' @title
#'  Check if ecr operator supports given representation.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Object of type \code{ecr_operator}.
#' @param representation [\code{character(1)}]\cr
#'   Representation as a string.
#' @return [\code{logical(1)}]
#'   \code{TRUE}, if operator supports the representation type.
#' @export
is.supported = function(operator, representation) {
  UseMethod("is.supported")
}

#' @export
is.supported.ecr_operator = function(operator, representation) {
  return (representation %in% getSupportedRepresentations(operator))
}
