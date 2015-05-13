#' Computation of the dominated hypervolume.
#'
#' Given a set of points \code{x}, this function computes the dominated hypervolume
#' of the points regarding the reference point \code{ref.point}. If the latter
#' is not provided, one is automatically determined by computing the maximum
#' in each dimension.
#'
#' @param x [\code{matrix}]\cr
#'   Matrix of points.
#' @param ref.point [\code{numeric} | \code{NULL}]\cr
#'   Reference point. Set to the maximum in each dimension by default if not provided.
#' @return [\code{numeric(1)}] Dominated hypervolume.
#' @export
computeDominatedHypervolume = function(x, ref.point = NULL) {
  # sanity checks
  assertMatrix(x, mode = "numeric", any.missing = FALSE)
  if (any(is.infinite(x))) {
    warningf("Set of points contains infinite %i values.", which(is.infinite(x)))
    return(NaN)
  }

  if (is.null(ref.point)) {
    ref.point = apply(x, 1, max)
  }

  if (length(ref.point) != nrow(x)) {
    stopf("Set of points and reference point need to have the same dimension, but
      set of points has dimension %i and reference points has dimension %i.", nrow(x), length(ref.point))
  }

  if (any(is.infinite(ref.point))) {
    warningf("Reference point contains infinite %i values.", which(is.infinite(ref.point)))
    return(NaN)
  }

  return(.Call("computeDominatedHypervolumeC", as.numeric(x), ref.point))
}
