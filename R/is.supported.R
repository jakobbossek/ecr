#' Helper function to check if esoo operator supports given representation.
#'
#' @param operator [\code{esoo_operator}]\cr
#'   Object of type \code{esoo_operator}.
#' @param representation [\code{character(1)}]\cr
#'   Representation as a string.
#' @return [\code{logical(1)}]
#'   \code{TRUE}, if operator supports the representation type.
#'
#' @export
is.supported = function(operator, representation) {
  UseMethod("is.supported")
}

#' @S3method is.supported esoo_operator
is.supported.esoo_operator = function(operator, representation) {
  return (representation %in% getSupportedRepresentations(operator))
}
