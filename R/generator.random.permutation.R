#' @title
#'   Generates a generator object for the initial population.
#'
#' @description
#'   This generator generates a population of permutations.
#'
#' @return [\code{ecr_operator}]
#' @export
makePermutationGenerator = function() {
  generatePermutationPopulation = function(size, task, control) {
    par.set = control$par.set
    n.params = sum(getParamLengths(par.set))
    population = list()
    for(i in seq(size)) {
      population[[i]] = sample(1:n.params)
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
