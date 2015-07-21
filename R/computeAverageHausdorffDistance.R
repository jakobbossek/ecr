#' Computes the Average Hausdorff Distance.
#'
#' @param A [\code{matrix}]\cr
#'   First point set (each row corresponds to a point).
#' @param B [\code{matrix}]\cr
#'   Second point set (each row corresponds to a point).
#' @param p [\code{numeric(1)}]\cr
#'   Parameter p of the average Hausdoff metrix. Default is 1. See the description
#'   for details.
#' @return [\code{numeric(1)}] Average Hausdorff distance of sets \code{A} and \code{B}.
#' @export
computeAverageHausdorffDistance = function(A, B, p = 1) {
  # sanity check imput
  assertMatrix(A, mode = "numeric", any.missing = FALSE)
  assertMatrix(B, mode = "numeric", any.missing = FALSE)
  if (ncol(A) != ncol(B)) {
    stopf("Sets A and B need to have the same dimensionality.")
  }
  assertNumber(p, lower = 0.0001, na.ok = FALSE)

  # ac
  GD = computeGenerationalDistance(A, B, p)
  IGD = computeGenerationalDistance(B, A, p)
  delta = max(GD, IGD)
  return(delta)
}

#' Helper to compute the Generational Distance (GD) between two sets of points.
#'
#' @param A [\code{matrix}]\cr
#'   First point set (each row corresponds to a point).
#' @param B [\code{matrix}]\cr
#'   Second point set (each row corresponds to a point).
#' @param p [\code{numeric(1)}]\cr
#'   Parameter p of the average Hausdoff metrix. Default is 1. See the description
#'   for details.
#' @return [\code{numeric(1)}]
#' @export
computeGenerationalDistance = function(A, B, p = 1) {
  dists = apply(A, 1L, function(a) computeDistanceFromPointToSetOfPoints, B)
  GD = mean(dists^p)^(1 / p)
  return(GD)
}

#' Helper to compute the Inverted Generational Distance (IGD) between two sets of points.
#'
#' @param A [\code{matrix}]\cr
#'   First point set (each row corresponds to a point).
#' @param B [\code{matrix}]\cr
#'   Second point set (each row corresponds to a point).
#' @param p [\code{numeric(1)}]\cr
#'   Parameter p of the average Hausdoff metrix. Default is 1. See the description
#'   for details.
#' @return [\code{numeric(1)}]
#' @export
computeInvertedGenerationalDistance = function(A, B, p = 1) {
  return(computeGenerationalDistance(B, A, p))
}

#' Helper to compute distance between a single point and a point set.
#'
#' @param a [\code{numeric(1)}]\cr
#'   Point given as a numeric vector.
#' @param B [\code{matrix}]\cr
#'   Point set (each row corresponds to a point).
#' @return [\code{numeric(1)}]
#' @export
computeDistanceFromPointToSetOfPoints = function(a, B) {
  # to avoid loops here we construct a matrix and make use of R's vector
  # computation qualities
  tmp = matrix(rep(a, each = nrow(B)), ncol = ncol(B), byrow = FALSE)
  dist = min(apply(tmp - B, 1L, function(x) {
    sqrt(sum(x^2))
  }))
  return(dist)
}
