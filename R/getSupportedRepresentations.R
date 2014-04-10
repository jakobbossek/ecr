#' @export
getSupportedRepresentations = function(operator) {
  UseMethod("getSupportedRepresentations")
}

#' @S3method getSupportedRepresentations esoo_operator
getSupportedRepresentations.esoo_operator = function(operator) {
  attr(operator, "supported")
}
