#' Helper function to check if ecr operator supports given representation.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Object of type \code{ecr_operator}.
#' @param representation [\code{character(1)}]\cr
#'   Representation as a string.
#' @return [\code{logical(1)}]
#'   \code{TRUE}, if operator supports the representation type.
#'
#' @export
is.supported = function(operator, representation) {
  UseMethod("is.supported")
}

#' @S3method is.supported ecr_operator
is.supported.ecr_operator = function(operator, representation) {
  return (representation %in% getSupportedRepresentations(operator))
}
