# @title
#  Assert same dimensions.
#
# @description
#   We frequently need to check whether the dimension of an approximation set is
#   equal to the dimension of the ideal point and/or nadir point. This does exactly
#   that assertion for an arbitrary number of arguments.
#
# @param ... [any]
#   Vectors or matrizes.
# @return Nothing. Stops if not all dimensions are equal.
assertSameDimensions = function(...) {
  xs = list(...)
  dims = sapply(xs, function(x) {
    # we store vectors one per column
    return(if (is.matrix(x)) nrow(x) else length(x))
  })
  if (!hasAllEqualElements(dims)) {
    stopf("All point sets and points need to be of the same dimension.")
  }
}

# @title
#   Check whether all elements of an vector are equal.
#
# @param x [vector]
#   Input vector.
# @return [logical(1)]
hasAllEqualElements = function(x) {
  return(length(unique(x)) == 1)
}

