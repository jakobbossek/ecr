#' Generates a generator object for the initial population.
#'
#' The generated operator samples uniformally distributed points in the design
#' space of the target function taking care not to violate bounds.
#'
#' @return [\code{ecr_operator}]
#' @export
makeBinaryGenerator = function() {
  generateBinaryPopulation = function(size, control) {
    par.set = control$par.set
    
    # a list of vectors is an individual
    createInd = function(param.length) {
      lapply(param.length, function(x) sample(c(0, 1), size = x, replace = TRUE))
    }
    population = lapply(seq(size), function(x) createInd(getParamLengths(par.set)))
    
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
