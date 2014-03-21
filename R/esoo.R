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
#' library(soobench)
#' f = generate_sphere_function(2)
#' res = esoo(f, max.iter = 50, pop.size = 10)
#' print(res$best.value)
#' @export
esoo = function(f, max.iter, pop.size) {
  # maybe make use of Bernds makeOptPathDf and ParamHelpers for this.

  n = number_of_parameters(f)
  population = generateRandomInitialPopulation(pop.size, n, lower_bounds(f), upper_bounds(f))
  catf("Initial Population generated.")
  population = computeFitness(population, f)
  best = getBestIndividual(population)
  trace = makeTrace(n)
  trace = addToTrace(trace, best, 0)

  i = 1L
  while (!isTerminiationCriterionFullfilled(i, max.iter)) {
    cat(".")
    parents = parentSelection(population, number.of.parents = 2, strategy = "best")
    #FIXME: how to add crossover params
    #FIXME: until now only one child generated
    children = recombinate(parents, type = "intermediate")
    children = gaussMutation(children)
    children = correctBounds(children, lower_bounds(f), upper_bounds(f))

    children = computeFitness(children, f)
    population = mergePopulations(population, children)

    # FIXME: elitism, survival of the fittest if (mu, lambda) strategy is used
    population = selectForSurvival(population, pop.size, strategy = "mupluslambda")

    best = getBestIndividual(population)
    trace = addToTrace(trace, best, i)

    i = i + 1
    #if (i == 3)
      #stopf("debug")
  }
  catf("\nEA finished!")
  return(
    structure(list(
      best.param = best$individual,
      best.value = best$fitness,
      trace = trace
      ), class = "esooResult")
  )
}