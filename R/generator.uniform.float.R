#' Generates a generator object for the initial population.
#'
#' @return [\code{ecr_opeator}]
#' @export
makeUniformGenerator = function() {
  generateUniformPopulation = function(size, control) {
    par.set = control$par.set
    lower = getLower(par.set)
    upper = getUpper(par.set)
    n.params = sum(getParamLengths(par.set))
    population = list()
    for(i in seq(size)) {
      ind = vector(mode = "numeric", length = n.params)
      for (j in seq(n.params)) {
        ind[j] = runif(1L, min = lower[j], max = upper[j])
      }
      population[[i]] = ind
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
