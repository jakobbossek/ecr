#' @title
#'   Generates a generator object for the initial population.
#'
#' @return [\code{ecr_generator}]
#' @export
makeUniformGenerator = function() {
  generateUniformPopulation = function(size, task, control) {
    par.set = control$par.set
    if (!isNumeric(par.set)) {
      stopf("Uniform generator needs a numeric parameter set.")
    }
    if (!hasFiniteBoxConstraints(par.set)) {
      stopf("Uniform generator needs box constraints.")
    }
    
    lower = getLower(par.set)
    upper = getUpper(par.set)
    
    # create one gene
    createGene = function(constr) {
      mapply(runif, n = 1L, min = constr$low, max = constr$up)
    }
    # is one gene an individual or is a set of genes an individual
    n.params = getParamNr(par.set)
    if (n.params == 1L) {
      constraints = list(low = lower, up = upper)
      createInd = createGene
    } else {
      ids = getParamIds(par.set)
      constraints = vector("list", n.params)
      names(constraints) = ids
      for (i.param in seq(n.params)) {
        constraints[[i.param]] = list(low = lower[names(lower) == ids[i.param]]
                                      , up = upper[names(upper) == ids[i.param]]
                                      )
      }
      # create individual from genes
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
