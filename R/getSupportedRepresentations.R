#' @export
getSupportedRepresentations = function(operator) {
  UseMethod("getSupportedRepresentations")
}

#' @S3method getSupportedRepresentations esoo_operator
getSupportedRepresentations = function(operator) {
  attr(operator, "supported")
}
