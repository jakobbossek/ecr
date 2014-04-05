#' Working horse of the esoo package.
#'
#' Takes a function and searches for global optimum with an evolutionary approach.
#'
#' @param f [\code{function}]\cr
#'   Target function.
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximum number of iterations.
#' @param pop.size [\code{integer(1)}]\cr
#'   Population size.
#' return [\code{esooResult}]
#'   Object of type \code{esooResult} containing a list:
#'   \itemize{
#'    \item{best.param \code{numeric}}{Best parameter combination.}
#'    \item{best.value \code{numeric(1)}}{Best reached value.}
#'    \item{trace \code{esooTrace}}{Optimization path.}
#'   }
#' @examples
#' \dontrun{
#' library(soobench)
#' f = generate_sphere_function(2)
#' res = esoo(f, max.iter = 50, pop.size = 10)
#' print(res$best.value)
#' }
#' @export
esoo = function(f, max.iter, pop.size) {
  #FIXME: add sanity checks
  #FIXME: add esooControl object. This object should be responsable to extensively sanity check
  #       all the input parameters and check the parameter combinations for validity

  #FIXME: until now only soobnech funs are supported. Force the user to offer a special type of
  #       funs (esoo_fitness_function)?
  n = number_of_parameters(f)
  #FIXME: generation of initial population is based on the type of parameters
  population = generateRandomInitialPopulation(pop.size, n, lower_bounds(f), upper_bounds(f))
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
    children = gaussMutation(children)
    children = correctBounds(children, lower_bounds(f), upper_bounds(f))

    children = computeFitness(children, f)
    population = mergePopulations(population, children)

    #FIXME: elitism, survival of the fittest if (mu, lambda) strategy is used
    population = selectForSurvival(population, pop.size, strategy = "mupluslambda")

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
