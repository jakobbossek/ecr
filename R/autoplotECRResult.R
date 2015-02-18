#' Plot optimization trace.
#'
#' @param object [\code{ecr_result}]\cr
#'   ecr result object.
#' @param xlim [\code{numeric(2)} | NULL]\cr
#'   Lower and upper bound for generation. If \code{NULL}, this is set automatically.
#' @param ylim [\code{numeric(2)} | NULL]\cr
#'   Lower and upper bound for fitness. If \code{NULL}, this is set automatically.
#' @param log.fitness [\code{logical(1)}]\cr
#'   Log-transform fitness values? Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @export
autoplot.ecr_result = function(object, xlim = NULL, ylim = NULL, log.fitness = FALSE, ...) {
  pl = autoplot(object$opt.path, xlim, ylim, log.fitness, ...)
  pl = pl + ggtitle(sprintf("Optimization trace for function '%s'", getName(object$objective.fun)))
  return (pl)
}

# autoplot function for opt.path used by ecr
autoplot.OptPath = function(object, xlim, ylim, log.fitness, ...) {
  ggdf = as.data.frame(object)
  ggdf = ggdf[c("dob", "pop.min.fitness", "pop.mean.fitness", "pop.median.fitness", "pop.max.fitness")]

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