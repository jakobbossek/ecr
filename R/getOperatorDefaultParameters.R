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
  return(NA)
}
