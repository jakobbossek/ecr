#' @title
#' Plot optimization trace.
#'
#' @description
#' Call this function on the result object of an \code{\link{doTheEvolution}}
#' function call to visualize the optimization trace.
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
#' @export
autoplot.ecr_single_objective_result = function(object, xlim = NULL, ylim = NULL, show.process = FALSE
                               , log.fitness = FALSE, complete.trace = FALSE, ...) {
  assertFlag(show.process, na.ok = FALSE)
  assertFlag(complete.trace, na.ok = FALSE)
  assertFlag(log.fitness, na.ok = FALSE)

  if (is.null(object$opt.path)) {
    stopf("Cannot plot optimization trace, since obviously no logging took place.")
  }

  op = object$opt.path
  op.df = as.data.frame(op, strings.as.factors = TRUE)
  # we start with the second dob, since otherwise there is not enough info to plot
  unique.dobs = unique(op.df$dob)[-1]
  if (complete.trace) {
    unique.dobs = tail(unique.dobs, 1)
  }
  for (dob in unique.dobs) {
    pl.trace = plotTrace(op.df[which(op.df$dob <= dob), ], xlim, ylim, log.fitness, ...)
    pl.trace = pl.trace + ggtitle("Optimization trace")
    if (show.process) {
      if (object$final.opt.state$control$representation == "custom") {
        stopf("Process cannot be visualized if custom representation was used.")
      }
      obj.fun = object$task$fitness.fun
      task = object$task
      par.set = smoof::getParamSet(obj.fun)
      n.params = getNumberOfParameters(obj.fun)

      if (n.params > 2L) {
        stopf("Visualization not possible for functions with more than 2 parameters.")
      }

      if (hasDiscrete(par.set)) {
        stopf("Visualization for mixed/discrete decision spaces not supported at the moment.")
      }

      if (isMultiobjective(obj.fun)) {
        stopf("Visualization not possible for multi-objective functions at the moment.")
      }

      # call smoof plot function
      pl.fun = autoplot(obj.fun)

      # get interesting stuff out of opt.path in ggplot2 friendly format
      df.points = getOptPathX(op, dob = dob)
      y.name = task$objective.names
      df.points[[y.name]] = getOptPathY(op, dob = dob)
      x.names = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)
      if (n.params == 2L) {
        pl.fun = pl.fun + geom_point(data = df.points, aes_string(x = x.names[1L], y = x.names[2L]), colour = "tomato")
      } else {
        pl.fun = pl.fun + geom_point(data = df.points, aes_string(x = x.names, y = y.name), colour = "tomato")
        opt.dir.fun = if (task$minimkze) min else max
        pl.fun = pl.fun + geom_hline(yintercept = opt.dir.fun(df.points[[y.name]]), linetype = "dashed", colour = "gray")
      }

      BBmisc::requirePackages(c("grid", "gridExtra"), why = "ecr")
      #FIXME: next line returns errors in 'test_autoplot.R'
      pl = do.call(gridExtra::grid.arrange, list(pl.fun, pl.trace, ncol = 1L))
      print(pl)
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
