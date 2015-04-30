# Helper functions for deciding and selecting no

#' Check if a vector dominates another vector.
#'
#' @keywords optimize
#'
#' @param x [\code{numeric}]\cr
#'   First vector.
#' @param y [\code{numeric}]\cr
#'   Second vector.
#' @return [\code{logical(1)}] Does \code{x} dominate \code{y}?
#' @export
dominates = function(x, y) {
  stopifnot(length(x) == length(y))
  return(all(x <= y) && any(x < y))
}

#' Check whether a vector is dominated by another vector.
#'
#' @keywords optimize
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

#' Check for pareto dominance.
#'
#' This function takes a numeric matrix \code{x} where each row corresponds to
#' a point and returns a logical vector. The i-th position of the latter is
#' \code{TRUE} if the i-th point is dominated by at least one other point.
#'
#' @param x [\code{matrix}]\cr
#'   Numeric (n x d) matrix where n is the number of points and d is the number
#'   of objectives.
#' @return [\code{logical}]
#' @export
dominated = function(x) {
  assertMatrix(x, min.rows = 2L, min.cols = 2L, any.missing = FALSE)
  n = nrow(x)
  dominated = logical(n)
  for (i in seq.int(n)) {
    for (j in seq.int(i, n)) {
      dominated[i] = dominated[i] || isDominated(x[i, ], x[j, ])
      dominated[j] = dominated[j] || isDominated(x[j, ], x[i, ])
    }
  }
  return(dominated)
}

#' Determine which points of a set are not (non)dominated.
#'
#' Simple wrapper functions around \code{\link{dominated}}. Given a matrix with one
#' point per row the \code{which.dominated} returns the row numbers of the dominated points
#' and \code{which.nondominated} the row numbers of the nondominated points.
#'
#' @keywords optimize
#'
#' @param x [\code{matrix}]\cr
#' Numeric (n x d) matrix where n is the number of points and d is the number
#'   of objectives.
#' @return [\code{integer}]
#' @examples
#' data(mtcars)
#' # assume we want to maximize horsepower and minimize gas consumption
#' cars = mtcars[, c("mpg", "hp")]
#' cars$hp = -cars$hp
#' idxs = which.nondominated(as.matrix(cars))
#' print(mtcars[idxs, ])
#' @rdname which.dominated
#' @export
which.dominated = function(x) {
  return(which(dominated(x)))
}

#' @rdname which.dominated
#' @export
which.nondominated = function(x) {
  return(which(!dominated(x)))
}
