#' @title
#'   Generates a generator object for the initial population.
#'
#' @return [\code{ecr_generator}]
#' @export
makePermutationGenerator = function() {
  generatePermutationPopulation = function(size, task, control) {
    par.set = control$par.set
    if (getParamNr(par.set) > 1L) {
      stopf("The Permutation Generator works only with one parameter(-vector).")
    }
    
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
