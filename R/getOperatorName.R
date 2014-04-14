#' Determine the name of a given operator.
#'
#' @param operator [\code{esoo_operator}]\cr
#'   Operator object.
#' @return [\code{character(1)}]
#'   Name of the operator.
#' @export
getOperatorName = function(operator) {
  UseMethod("getOperatorName")
}

#' @S3method getOperatorName esoo_operator
getOperatorName.esoo_operator = function(operator) {
  attr(operator, "name")
}
