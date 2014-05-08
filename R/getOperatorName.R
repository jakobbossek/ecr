#' Determine the name of a given operator.
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
