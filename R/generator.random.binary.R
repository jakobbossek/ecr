#' @title
#'   Generates a generator object for the initial population.
#'
#' @description
#'   The generated operator samples uniformally distributed points in the design
#'   space of the target function taking care not to violate bounds.
#'
#' @return [\code{ecr_operator}]
#'
#' @export
makeBinaryGenerator = function() {
  generateBinaryPopulation = function(size, control) {
    par.set = control$par.set
    n.params = sum(getParamLengths(par.set))
    population = list()
    for (i in seq(size)) {
      population[[i]] = sample(c(0, 1), size = n.params, replace = TRUE)
    }
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
