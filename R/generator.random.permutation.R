#' Generates a generator object for the initial population.
#'
#' @return [\code{ecr_operator}]
#' @export
makePermutationGenerator = function() {
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
  generatePermutationPopulation = function(size, n.params, lower.bounds = NA, upper.bounds = NA, control) {
    population = matrix(0, nrow = size, ncol = n.params)
    for(i in 1:size) {
      population[i, ] = sample(1:n.params)
    }
    makePopulation(population)
  }
  operator = makeOperator(
    operator = generatePermutationPopulation,
    name = "Permutation generator",
    description = "Generates random permutations.",
    supported = c("permutation")
  )
  operator = addClasses(operator, c("ecr_generator"))
  return(operator)
}
