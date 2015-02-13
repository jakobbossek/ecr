#' Plot optimization trace.
#'
#' @param object [\code{ecr_result}]\cr
#'   ecr result object.
#' @param xlim [\code{numeric(2)} | NULL]\cr
#'   Lower and upper bound for generation. If \code{NULL}, this is set automatically.
#' @param ylim [\code{numeric(2)} | NULL]\cr
#'   Lower and upper bound for fitness. If \code{NULL}, this is set automatically.
#' @param ... [any]\cr
#'   Not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @export
autoplot.ecr_result = function(object, xlim = NULL, ylim = NULL, ...) {
  ggdf = as.data.frame(object$opt.path)
  ggdf = ggdf[c("dob", "pop.min.fitness", "pop.mean.fitness", "pop.median.fitness", "pop.max.fitness")]

  xlim = BBmisc::coalesce(xlim, c(0, max(ggdf$dob)))
  ylim = BBmisc::coalesce(ylim, c(0, max(ggdf$pop.max.fitness)))
  assertNumeric(ylim, len = 2L, any.missing = FALSE)
  assertNumeric(xlim, len = 2L, any.missing = FALSE)

  requirePackages("reshape2", why = "ecr")
  ggdf = melt(ggdf, c("dob"))
  ggdf$variable = as.factor(ggdf$variable)

  pl = ggplot(data = ggdf, mapping = aes_string(x = "dob", y = "value", linetype = "variable"))
  pl = pl + geom_line()
  pl = pl + xlab("Generation") + ylab("Fitness")
  pl = pl + xlim(xlim) + ylim(ylim)
  pl = pl + scale_linetype_discrete(name = "Type")
  pl = pl + ggtitle(sprintf("Optimization trace for function '%s'", getName(object$objective.fun)))

  return(pl)
}
