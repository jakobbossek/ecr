#' Returns the parameter check function of a given operator.
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
