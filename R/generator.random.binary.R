#' Generates a generator object for the initial population.
#'
#' @return [\code{ecr_operator}]
#' @export
makeBinaryGenerator = function() {
  # Generates an initial population.
  #
  # Samples uniformally distributed points in the design space of the target function
  # taking care not to violate bounds.
  #
  # @param size [\code{integer(1)}]\cr
  #   Number of points to generate.
  # @param n.params [\code{integer(1)}]\cr
  #   Number of parameter of the target function.
  # @param lower.bounds [\code{numeric}]\cr
  #   Vector of size \code{n.params} indicating the lower bounds for each dimension.
  # @param upper.bounds [\code{numeric}]\cr
  #   Vector of size \code{n.params} indicating the upper bounds for each dimension.
  # @param control [\code{ecr_control}]\cr
  #   Control object.
  # @return [\code{setOfIndividuals}]
  generateBinaryPopulation = function(size, n.params, lower.bounds = NA, upper.bounds = NA, control) {
    population = matrix(sample(c(0,1), size = n.params * size, replace = TRUE), nrow = size, ncol = n.params)
    makePopulation(population)
  }
  operator = makeOperator(
    operator = generateBinaryPopulation,
    name = "Binary generator",
    description = "Samples uniformally distributed 0, 1 values.",
    supported = c("binary")
  )
  operator = addClasses(operator, c("ecr_generator"))
  return(operator)
}
