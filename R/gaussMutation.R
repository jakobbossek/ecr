# Mutate individuals.
#
# @param setOfIndividuals [\code{setOfIndividuals}]\cr
#   Set of individuals.
# @param prob [\code{numeric(1)}]\cr
#   Probability of mutation. Must be in (0,1).
# @return [\code{setOfIndividuals}]
#   Mutated set of individuals.
gaussMutation = function(setOfIndividuals, prob = 0.1) {
  #FIXME: each mutation operator has its own parameters. Maybe better offer generic 'params' parameter.
  n.params = ncol(setOfIndividuals$population)
  n = nrow(setOfIndividuals$population)
  for (i in seq(n)) {
    mutation.bool = (runif(n.params) <= 0.1)
    mutation = ifelse(mutation.bool, rnorm(1, mean = 0, sd = 0.2), 0)
    setOfIndividuals$population[i, ] = setOfIndividuals$population[i, ] + mutation
  }
  return(setOfIndividuals)
}
