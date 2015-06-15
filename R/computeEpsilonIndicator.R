#' Computation of the unary epsilon-indicator.
#'
#' Given a set of points \code{x}, this function computes the unary epsilon-indicator
#' provided a set of reference points \code{ref.points}.
#'
#' @param x [\code{matrix}]\cr
#'   Matrix of points.
#' @param ref.points [\code{matrix}]\cr
#'   Set of reference points.
#' @return [\code{numeric(1)}] Epsilon indicator.
#' @export
computeEpsilonIndicator = function(x, ref.points) {
  # sanity checks
  assertMatrix(x, mode = "numeric", any.missing = FALSE)
  assertMatrix(ref.points, mode = "numeric", any.missing = FALSE)

  if (nrow(ref.points) != nrow(x)) {
    stopf("Set of points and reference points need to have the same dimension, but
      set of points has dimension %i and reference points has dimension %i.", nrow(x), nrow(ref.points))
  }

  return(.Call("calculateEpsilonIndicatorFromR", x, ref.points))
}
