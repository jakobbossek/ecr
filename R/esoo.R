#' Working horse of the esoo package.
#'
#' Takes a function and searches for global optimum with an evolutionary approach.
#'
#' @param f [\code{function}]\cr
#'   Target function.
#' @param control [\code{esoo.control}]\cr
#'   Control object.
#' @param global.optimum [\code{global.optimum}]\cr
#'   Parameter combination of the global optimum of the fun. This parameter is optional.
#' return [\code{esooResult}]
#'   Object of type \code{esooResult} containing a list:
#'   \itemize{
#'    \item{best.param \code{numeric}}{Best parameter combination.}
#'    \item{best.value \code{numeric(1)}}{Best reached value.}
#'    \item{trace \code{esooTrace}}{Optimization path.}
#'   }
#' @export
esoo = function(f, control, global.optimum = NA) {
  n = control$n.params
  max.iter = control$max.iter
  population.size = control$population.size
  show.info = control$show.info
  show.info.stepsize = control$show.info.stepsize
  termination.eps = control$termination.eps

  if (!any(is.na(global.optimum))) {
    if (length(global.optimum) != control$n.params) {
      stopf("Given global optimum %s suggests %i parameters, but objective function has %i parameters.",
        paste("(", strImplode(global.optimum, sep = ","), ")", sep=""), length(global.optimum), control$n.params)
    }
  }

  mutator = control$mutator
  recombinator = control$recombinator

  population = generateRandomInitialPopulation(population.size, n, lower_bounds(f), upper_bounds(f))
  population = computeFitness(population, f)
  best = getBestIndividual(population)
  trace = makeTrace(n)
  trace = addToTrace(trace, best, 0)

  i = 1L
  while (!isTerminiationCriterionFullfilled(i, max.iter, global.optimum, best, termination.eps)) {
    if (show.info) {
      if (i %% show.info.stepsize == 0L) {
        cat(".")
      }
    }
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
  }
  catf("\nEA finished!")

  return(
    structure(list(
      best.param = best$individual,
      best.value = best$fitness,
      trace = trace
      ), class = "esoo_result")
  )
}
