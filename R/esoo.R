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
  n = control$n.params
  max.iter = control$max.iter
  population.size = control$population.size

  mutator = control$mutator
  recombinator = control$recombinator

  population = generateRandomInitialPopulation(population.size, n, lower_bounds(f), upper_bounds(f))
  catf("Initial Population generated.")
  population = computeFitness(population, f)
  best = getBestIndividual(population)
  trace = makeTrace(n)
  trace = addToTrace(trace, best, 0)

  i = 1L
  while (!isTerminiationCriterionFullfilled(i, max.iter)) {
    cat(".")
    parents = parentSelection(population, number.of.parents = 2)
    children = recombinator(parents)
    children = mutator(children, control)
    children = correctBounds(children, lower_bounds(f), upper_bounds(f))

    children = computeFitness(children, f)
    population = mergePopulations(population, children)

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
