#' Generates a generator object for the initial population.
#'
#' @return [\code{ecr_opeator}]
#' @export
makeUniformGenerator = function() {
  generateUniformPopulation = function(size, control) {
    par.set = control$par.set
    if (!isNumeric(par.set)) {
      stopf("Uniform generator needs a numeric parameter set.")
    }
    if (!hasFiniteBoxConstraints(par.set)) {
      stopf("Uniform generator needs box constraints.")
    }
    
    lower = getLower(par.set)
    upper = getUpper(par.set)
    createGene = function(constr) {
      mapply(runif, n = 1L, min = constr[, 1], max = constr[, 2])
    }
    if (getParamNr(par.set) == 1L) {
      constraints = cbind(lower, upper)
      createInd = createGene
    } else {
      ids = getParamIds(par.set)
      constraints = vector("list", getParamNr(par.set))
      names(constraints) = ids
      for (i.param in seq(getParamNr(par.set))) {
        constraints[[i.param]] = cbind(lower = lower[names(lower) == ids[i.param]]
                                       , upper = upper[names(upper) == ids[i.param]]
                                       )
      }
      createInd = function(constr) {
        lapply(constr, createGene)
      }
    }

    population = lapply(seq(size), function(x) createInd(constraints))
    makePopulation(population)
  }
  generator = makeGenerator(
    generator = generateUniformPopulation,
    name = "Uniform generator",
    description = "Samples uniformally distributed points in the design space.",
    supported = "float"
  )
  return(generator)
}
