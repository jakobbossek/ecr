#' Generates a generator object for the initial population.
#'
#' @return [\code{ecr_operator}]
#' @export
makePermutationGenerator = function() {
  generatePermutationPopulation = function(size, control) {
    par.set = control$par.set
    n.params = sum(getParamLengths(par.set))
    population = list()
    for(i in seq(size)) {
      population[[i]] = sample(1:n.params)
    }
    makePopulation(population)
  }
  generator = makeGenerator(
    generator = generatePermutationPopulation,
    name = "Permutation generator",
    description = "Generates random permutations.",
    supported = c("permutation")
  )
  return(generator)
}
