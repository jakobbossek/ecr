#' Generates a generator object for the initial population.
#'
#' @return [\code{ecr_opeator}]
#' @export
makeUniformGenerator = function() {
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
  generateUniformPopulation = function(size, n.params, lower.bounds = NA, upper.bounds = NA, control) {
    population = matrix(0, nrow = size, ncol = n.params)
    for(i in 1:n.params) {
      population[, i] = runif(size, min = lower.bounds[i], max = upper.bounds[i])
    }
    makePopulation(population)
  }
  operator = makeOperator(
    operator = generateUniformPopulation,
    name = "Uniform generator",
    description = "Samples uniformally distributed points in the design space.",
    supported = "float"
  )
  operator = addClasses(operator, c("ecr_generator"))
  return(operator)
}
