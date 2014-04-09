#' @export
getOperatorName = function(operator) {
  UseMethod("getOperatorName")
}

#' @S3method getOperatorName esoo_operator
getOperatorName.esoo_operator = function(operator) {
  attr(operator, "name")
}
