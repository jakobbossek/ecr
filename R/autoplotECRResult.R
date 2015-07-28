#' @title
#'   Plot optimization trace.
#'
#' @description
#'   Call this function on the result object of an \code{\link{doTheEvolution}} function
#'   call to visualize the optimization trace.
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
#' @param complete.trace [\code{logical(1)}]\cr
#'   Direct show the plot with the fitness for all generations. Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Not used.
#' @return [\code{invisible(TRUE)}]
#'
#' @export
autoplot.ecr_single_objective_result = function(object, xlim = NULL, ylim = NULL, show.process = FALSE
                               , log.fitness = FALSE, complete.trace = FALSE, ...) {
  assertFlag(show.process, na.ok = FALSE)
  assertFlag(complete.trace, na.ok = FALSE)
  obj.fun = object$objective.fun
  n.params = getNumberOfParameters(obj.fun)

  op = as.data.frame(object$opt.path)
  # we start with the second dob, since otherwise there is not enough info to plot
  unique.dobs = unique(op$dob)[-1]
  if (complete.trace) {
    unique.dobs = tail(unique.dobs, 1)
  }
  for (dob in unique.dobs) {
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
      population = object$population.storage[[paste0("gen.", dob)]]
      if (n.params == 2L) {
        df.points = as.data.frame(do.call(rbind, population$individuals))
        colnames(df.points) = paste("x", 1:n.params, sep = "")
        df.points$y = as.numeric(population$fitness)
        pl.fun = pl.fun + geom_point(data = df.points, aes_string(x = "x1", y = "x2"), colour = "tomato")
      } else {
        fitness = as.numeric(population$fitness)
        df.points = data.frame(x = do.call(c, population$individuals), y = fitness)
        pl.fun = pl.fun + geom_point(data = df.points, aes_string(x = "x", y = "y"), colour = "tomato")
        pl.fun = pl.fun + geom_hline(yintercept = min(fitness), linetype = "dashed", colour = "gray")
      }

      #FIXME: this seems to fail!
      BBmisc::requirePackages(c("grid", "gridExtra"), why = "ecr")
      #FIXME: next line returns errors in 'test_autoplot.R'
      pl = do.call(gridExtra::arrangeGrob, list(pl.fun, pl.trace, ncol = 1))
      #pl = pl.trace
    } else {
      pl = pl.trace
    }

    print(pl)
    if (dob != tail(unique.dobs, 1))
      pause()
  }
  return(invisible(TRUE))
}

# autoplot function for opt.path used by ecr
plotTrace = function(df, xlim, ylim, log.fitness, ...) {
  ggdf = df[c("dob", "pop.min.fitness", "pop.mean.fitness", "pop.median.fitness", "pop.max.fitness")]
  xlim = BBmisc::coalesce(xlim, c(0, max(ggdf$dob)))
  ylim = BBmisc::coalesce(ylim, c(min(ggdf$pop.min.fitness), max(ggdf$pop.max.fitness)))
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
