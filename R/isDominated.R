#' Check whether a vector is dominated by another vector.
#'
#' @param x [\code{numeric}]\cr
#'   First vector.
#' @param y [\code{numeric}]\cr
#'   Second vector.
#' @return [\code{logical(1)}] Returns \code{TRUE} if \code{x} is dominated by
#'   \code{y} and \code{FALSE} otherwise.
#' @export
isDominated = function(x, y) {
  return(dominates(y, x))
}
