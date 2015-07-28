#' @title
#'   Non-dominated sorting algorithm.
#'
#' @description
#'   Implementation of the fast non-dominated sorting algorithm proposed by Deb.
#'
#' @param x [\code{matrix}]\cr
#'   Numeric matrix of points. Each row contains on points.
#' @return [\code{list}]
#'   List with the following components
#'   \describe{
#'     \item{ranks}{Integer vector of ranks of length \code{nrow(x)}. The higher
#'     the rank, the higher the domination front the corresponding points is
#'     located on.}
#'     \item{dom.counter}{Integer vector of length \code{nrow(x)}. The i-th element
#'     is the domination counter / domination number of the i-th point.}
#'   }
#'
#' @export
#FIXME: [later] implement this in C(++)
doNondominatedSorting = function(x) {
  # initialize domination front wrapper
  fronts = list()
  fronts[[1L]] = list()

  n = nrow(x)
  dom.counter = integer(n)
  ranks = integer(n)
  dom.els = vector(mode = "list", length = n)

  # compute domination numbers and pareto front
  for (i in seq.int(n)) {
    for (j in seq.int(n)) {
      if (dominates(x[i, ], x[j, ])) {
        dom.els[[i]] = c(dom.els[[i]], j)
      } else if (isDominated(x[i, ], x[j, ])) {
        dom.counter[i] = dom.counter[i] + 1L
      }
    }
    # in this case point x_i belongs to the pareto front, i.e., domination layer 1
    if (dom.counter[i] == 0L) {
      ranks[i] = 1L
      fronts[[1L]] = c(fronts[[1L]], i)
    }
  }

  # make a copy of the dominations number since we are going to modify these
  # in the next lines, but also want to return them
  dom.counter2 = dom.counter

  # now compute the remaining domination fronts
  k = 1L
  while (length(fronts[[k]]) > 0L) {
    front2 = list()
    for (i in fronts[[k]]) {
      for (j in dom.els[[i]]) {
        dom.counter[j] = dom.counter[j] - 1L
        if (dom.counter[j] == 0L) {
          ranks[j] = k + 1L
          front2 = c(front2, j)
        }
      }
    }
    k = k + 1L
    fronts[[k]] = front2
  }

  return(list(
    ranks = ranks,
    dom.counter = dom.counter2
  ))
}
