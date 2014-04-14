#' Returns the representations which a specific operator supports.
#'
#' Operators and representation types are not mandatory compatible. This function
#' detemines the types an operator can operate on.
#'
#' @param operator [\code{esoo_operator}]\cr
#'   Operator object.
#' @return [\code{character}]
#'   Vector of representation types.
#' @export
getSupportedRepresentations = function(operator) {
  UseMethod("getSupportedRepresentations")
}

#' @S3method getSupportedRepresentations esoo_operator
getSupportedRepresentations.esoo_operator = function(operator) {
  attr(operator, "supported")
}
