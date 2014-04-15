#' Working horse of the esoo package.
#'
#' Takes a function and searches for global optimum with an evolutionary approach.
#'
#' @param f [\code{function}]\cr
#'   Target function.
#' @param control [\code{esoo.control}]\cr
#'   Control object.
#' @param global.optimum [\code{numeric}]\cr
#'   Parameter combination of the global optimum of the fun. This parameter is optional.
#' @param lower [\code{numeric}]\cr
#'   Lower box constraints for the parameters. These are needed, if representation type is set
#'   to 'float'.
#' @param upper [\code{numeric}]\cr
#'   Upper box constraints for the parameters. These are needed, if representation type is set
#'   to 'float'.
#' return [\code{esooResult}]
#'   Object of type \code{esooResult} containing a list:
#'   \itemize{
#'    \item{best.param \code{numeric}}{Best parameter combination.}
#'    \item{best.value \code{numeric(1)}}{Best reached value.}
#'    \item{trace \code{esooTrace}}{Optimization path.}
#'   }
#' @export
esoo = function(f, control, global.optimum = NA, lower = NA, upper = NA) {
  n.params = control$n.params
  max.iter = control$max.iter
  population.size = control$population.size
  show.info = control$show.info
  show.info.stepsize = control$show.info.stepsize
  termination.eps = control$termination.eps

  #FIXME: maybe better outsource the sanity checks to dedicated function
  if (!any(is.na(global.optimum))) {
    if (length(global.optimum) != control$n.params) {
      stopf("Given global optimum %s suggests %i parameters, but objective function has %i parameters.",
        paste("(", strImplode(global.optimum, sep = ","), ")", sep=""), length(global.optimum), control$n.params)
    }
  }

  if (is.na(lower) && is.na(upper) && is_soo_function(f)) {
    lower = lower_bounds(f)
    upper = upper_bounds(f)
  }

  if ((is.na(lower) || is.na(upper)) && control$representation %in% c("float")) {
    stopf("Lower and upper box constraints needed for representation type 'float'.")
  }

  generator = control$generator
  mutator = control$mutator
  recombinator = control$recombinator

  population = generator(population.size, n.params, lower, upper)
  population = computeFitness(population, f)
  best = getBestIndividual(population)
  trace = makeTrace(n.params)
  trace = addToTrace(trace, best, 0)

  i = 1L
  while (!isTerminiationCriterionFullfilled(i, max.iter, global.optimum, best, termination.eps)) {
    if (show.info && (i %% show.info.stepsize == 0L)) {
      cat(".")
    }
    parents = parentSelection(population, number.of.parents = 2)
    children = recombinator(parents)
    children = mutator(children, control)
    children = correctBounds(children, lower, upper)

    children = computeFitness(children, f)
    population = mergePopulations(population, children)

    population = selectForSurvival(population, population.size, strategy = "mupluslambda")

    best = getBestIndividual(population)
    trace = addToTrace(trace, best, i)

    i = i + 1
  }

  return(
    structure(list(
      best.param = best$individual,
      best.value = best$fitness,
      trace = trace
      ), class = "esoo_result")
  )
}
