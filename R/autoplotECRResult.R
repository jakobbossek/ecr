#' Plot optimization trace.
#'
#' Call this function on the result object of an \code{\link{ecr}} function call
#' to visualize the optimization trace.
#'
#' @param object [\code{ecr_result}]\cr
#'   ecr result object.
#' @param xlim [\code{numeric(2)} | NULL]\cr
#'   Lower and upper bound for generation. If \code{NULL}, this is set automatically.
#' @param ylim [\code{numeric(2)} | NULL]\cr
#'   Lower and upper bound for fitness. If \code{NULL}, this is set automatically.
#' @param show.process [\code{logical(1)}]\cr
#'   Should the function itself with the population be plotted as well? Default is
#'   \code{FALSE}.
#' @param log.fitness [\code{logical(1)}]\cr
#'   Log-transform fitness values? Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Not used.
#' @return Nothing.
#' @export
autoplot.ecr_result = function(object, xlim = NULL, ylim = NULL, show.process = FALSE, log.fitness = FALSE, ...) {
  assertFlag(show.process, na.ok = FALSE)
  obj.fun = object$objective.fun
  n.params = getNumberOfParameters(obj.fun)

  op = as.data.frame(object$opt.path)
  unique.dobs = unique(op$dob)
  # we start with the second dob, since otherwise there is not enough info to plot
  for (dob in unique.dobs[2:length(unique.dobs)]) {
    pl.trace = plotTrace(op[which(op$dob <= dob), ], xlim, ylim, log.fitness, ...)
    pl.trace = pl.trace + ggtitle(sprintf("Optimization trace for function '%s'", getName(object$objective.fun)))
    if (show.process) {
      if (n.params > 2L || isMultiobjective(obj.fun)) {
        stopf("Visualization not possible for multi-objective functions or functions with greater than 2 parameters.")
      }
      if (!length(object$control$save.population.at)) {
        stopf("Cannot visualize population since no population was stored! Take a glance a the 'save.population.at' control parameter.")
      }
      pl.fun = autoplot(obj.fun)
      population = object$population.storage[[as.character(dob)]]
      df.points = data.frame(x = population$individuals, y = population$fitness)
      pl.fun = pl.fun + geom_point(data = df.points, aes_string(x = "x", y = "y"), colour = "tomato")
      pl.fun = pl.fun + geom_hline(yintercept = min(population$fitness), linetype = "dashed", colour = "gray")
    }
    if (show.process) {
      #FIXME: this seems to fail!
      BBmisc::requirePackages(c("grid", "gridExtra"), why = "ecr")
      pl = do.call(gridExtra::arrangeGrob, list(pl.fun, pl.trace, ncol = 1))
    } else {
      pl = pl.trace
    }
    pause()
    print(pl)
  }
}

# autoplot function for opt.path used by ecr
plotTrace = function(df, xlim, ylim, log.fitness, ...) {
  ggdf = df[c("dob", "pop.min.fitness", "pop.mean.fitness", "pop.median.fitness", "pop.max.fitness")]
  xlim = BBmisc::coalesce(xlim, c(0, max(ggdf$dob)))
  ylim = BBmisc::coalesce(ylim, c(0, max(ggdf$pop.max.fitness)))
  assertNumeric(ylim, len = 2L, any.missing = FALSE)
  assertNumeric(xlim, len = 2L, any.missing = FALSE)
  assertFlag(log.fitness)

  requirePackages("reshape2", why = "ecr")
  ggdf = melt(ggdf, c("dob"))
  ggdf$variable = as.factor(ggdf$variable)

  pl = ggplot(data = ggdf, mapping = aes_string(x = "dob", y = "value", linetype = "variable"))
  pl = pl + geom_line()
  pl = pl + xlab("Generation") + ylab("Fitness")
  pl = pl + xlim(xlim) + ylim(ylim)
  pl = pl + scale_linetype_discrete(name = "Type")

  if (log.fitness) {
    pl = pl + scale_y_log10()
    pl = pl + ylab("log(Fitness)")
  }

  return(pl)
}
