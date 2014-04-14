#' Working horse of the esoo package.
#'
#' Takes a function and searches for global optimum with an evolutionary approach.
#'
#' @param f [\code{function}]\cr
#'   Target function.
#' @param control [\code{esoo.control}]\cr
#'   Control object.
#' return [\code{esooResult}]
#'   Object of type \code{esooResult} containing a list:
#'   \itemize{
#'    \item{best.param \code{numeric}}{Best parameter combination.}
#'    \item{best.value \code{numeric(1)}}{Best reached value.}
#'    \item{trace \code{esooTrace}}{Optimization path.}
#'   }
#' @export
esoo = function(f, control) {
  #FIXME: until now only soobnech funs are supported. Force the user to offer a special type of
  #       funs (esoo_fitness_function)?
  n = control$n.params
  max.iter = control$max.iter
  population.size = control$population.size
  #FIXME: generation of initial population is based on the type of parameters
  population = generateRandomInitialPopulation(population.size, n, lower_bounds(f), upper_bounds(f))
  #FIXME: add show.info/monitoring option to allow the used (de)activation of messages
  #       Look for a logging package in R
  catf("Initial Population generated.")
  population = computeFitness(population, f)
  best = getBestIndividual(population)
  #FIXME: maybe use ParamHelpers::makeOptPathDF for that?
  trace = makeTrace(n)
  trace = addToTrace(trace, best, 0)

  i = 1L
  while (!isTerminiationCriterionFullfilled(i, max.iter)) {
    #FIXME: maybe add something like 'show.info.stepsize' to control in
    #       which iteration we get additional output
    cat(".")
    parents = parentSelection(population, number.of.parents = 2)
    #FIXME: how to add crossover params?
    #FIXME: until now only one child generated
    #FIXME: recombination, mutation and so on are all based on the representation of the individuals
    children = recombinate(parents, type = "intermediate")
    children = control$mutator(children, control)
    children = correctBounds(children, lower_bounds(f), upper_bounds(f))

    children = computeFitness(children, f)
    population = mergePopulations(population, children)

    #FIXME: elitism, survival of the fittest if (mu, lambda) strategy is used
    population = selectForSurvival(population, population.size, strategy = "mupluslambda")

    best = getBestIndividual(population)
    trace = addToTrace(trace, best, i)

    i = i + 1
    #if (i == 3)
      #stopf("debug")
  }
  catf("\nEA finished!")
  #FIXME: add S3 plot function for trace
  #FIXME: add print method for esooResult
  return(
    structure(list(
      best.param = best$individual,
      best.value = best$fitness,
      trace = trace
      ), class = "esooResult")
  )
}
