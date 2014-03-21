# Corrects the parameters if they are out of bounds.
#
# @param individuals [\code{setOfIndividuals}]\cr
#   Set of individuals, for example the entire population.
# @param lower.bounds [\code{numerci}]\cr
#   Vector of lower bounds for each dimension.
# @param upper.bounds [\code{numerci}]\cr
#   Vector of upper bounds for each dimension.
# @return [\code{setOfIndividuals}].
correctBounds = function(individuals, lower.bounds, upper.bounds) {
  for (i in 1:nrow(individuals$population)) {
    for (j in 1:ncol(individuals$population)) {
      if (individuals$population[i, j] < lower.bounds[j]) {
        individuals$population[i, j] = lower.bounds[j]
      } else if (individuals$population[i, j] > upper.bounds[j]) {
        individuals$population[i, j] = upper.bounds[j]
      }
    }
  }
  return(individuals)
}