#' @title
#'   Generates the uniform generator object for the initial population.
#'
#' @description
#'   The returned population contains individuals which are uniformly distributed
#'   within the bounds specified by the paramter set of the \pkg{smoof} objective
#'   function passed to the \code{\link{doTheEvolution}} function.
#'
#' @return [\code{ecr_opeator}]
#' @export
makeUniformGenerator = function() {
  generateUniformPopulation = function(size, task, control) {
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
