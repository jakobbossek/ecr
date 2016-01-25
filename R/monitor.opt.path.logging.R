#' @title
#' Logging monitor.
#'
#' @description
#' This is the default logging function used by ecr. It makes use of the fantastic
#' \code{\link[ParamHelpers]{OptPath}} which allows optimizers to log evaluated
#' points.
#' The logger is registered to be called once the EA is initialized (after the
#' creation of the initial population), right after each survival selection and
#' once the EA finished. By default the logger stores the entire population, i.e.,
#' the individuals, the fitness values and moreover some additional statistics
#' of the fitness value distribution.
#'
#' A word of causion: In case of a custom representation, which can not be
#' stored in the \code{\link[ParamHelpers]{OptPath}} by default since the
#' individuals are more complex than normal parameters, the serialized individuals
#' are stored in the \code{\link[ParamHelpers]{OptPath}}.
#'
#' @param step [\code{integer(1)}]\cr
#'   After how many generations should the logger be active?
#'   Default is 1, which means that logging takes place in each generation.
#' @param log.extras.fun [\code{function} | \code{NULL}]\cr
#'   Function which expects the internal optimization state \code{opt.state} and
#'   additional parameters \code{...} and returns a numeric vector of scalar
#'   qualities computed based e.g. on the individual genomes or the fitness values.
#'   The function is called every time the \code{logger} is called in each generation.
#'   The results are then stored additionally in the \code{\link[ParamHelpers]{OptPath}}.
#'   Default is \code{NULL}, which means no extra logging.
#' @return [\code{ecr_monitor}]
#' @export
setupOptPathLoggingMonitor = function(step = 1L, log.extras.fun = NULL) {
  # sanity checks
  assertInteger(step, len = 1L, lower = 1L, upper = 100L, any.missing = FALSE)
  if (!is.null(log.extras.fun)) {
    assertFunction(log.extras.fun, args = c("opt.state", "..."), ordered = TRUE)
  }

  force(step)
  force(log.extras.fun)

  makeMonitor(
    before = function(opt.state, ...) {
      task = opt.state$task
      par.set = opt.state$par.set # the task builds the par.set
      opt.state$opt.path = makeOptPathDF(par.set, y.names = task$objective.names,
        minimize = task$minimize,
        include.extra = TRUE, include.exec.time = FALSE
      )

      # now add initial population
      extras = getListOfExtras(opt.state, log.extras.fun)
      addIndividualsToOptPath(opt.state, extras)
    },
    step = function(opt.state, ...) {
      iter = opt.state$iter

      if ((iter %% step) == 0L) {
        # extract data
        task = opt.state$task

        # handle extra logging stuff
        extras = getListOfExtras(opt.state, log.extras.fun)
        addIndividualsToOptPath(opt.state, extras)
      }
    },
    after = function(opt.state, ...) {
      # nothing to do here
    }
  )
}

# @title
# Actually add individuals to OptPath.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Current iteration/generation.
addIndividualsToOptPath = function(opt.state, extras) {
  par.set = opt.state$task$par.set

  individuals = opt.state$population$individuals
  fitness = opt.state$population$fitness

  n.population = ncol(fitness)
  n.pars = length(par.set$pars)

  # now iterate over the population and store all individuals
  for (i in seq(n.population)) {
    x = individuals[[i]]
    if (opt.state$control$representation == "custom") {
      # serialization of custom represetation
      x = serializeIndividual(x)
    }
    if (n.pars == 1L) {
      x = list(x)
    }
    y = fitness[, i]
    addOptPathEl(opt.state$opt.path, x = x, y = y, dob = opt.state$iter,
      extra = extras, check.feasible = FALSE
    )
  }
  invisible()
}

# @title
# Generate 'extras' argument for opt.path.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Current iteration/generation.
# @param log.extras.fun [\code{function} | \code{NULL}]\cr
#  Function which computes additional logging stuff.
# @return [list] Named list with scalar values to be stored in opt.path.
getListOfExtras = function(opt.state, log.extras.fun = NULL) {
  fitness = opt.state$population$fitness
  extras = list(
    past.time = as.numeric(Sys.time() - opt.state$time.created),
    n.evals = opt.state$n.evals,
    pop.min.fitness = min(fitness),
    pop.mean.fitness = mean(fitness),
    pop.median.fitness = median(fitness),
    pop.max.fitness = max(fitness)
  )
  # compute and log used defined stuff
  if (!is.null(log.extras.fun)) {
    user.extras = log.extras.fun(opt.state)
    if (!testList(user.extras, names = "strict")) {
      stopf("Result computed by 'extras.fun' is not a named list!")
    }
    extras = c(extras, user.extras)
  }
  return(extras)
}

# @title
# Serialize custom individual to character string.
#
# @param x [\code{any}]\cr
#   Individual.
# @return [\code{character(1)}] Serialized individual.
serializeIndividual = function(x) {
  return(rawToChar(serialize(x, connection = NULL, ascii = TRUE)))
}

# @title
# Unserialize custom individual to its original form.
#
# @param x [\code{character(1L)}]\cr
#   Serialized individual.
# @return [\code{any}] Unserialized individual.
unserializeIndividual = function(x) {
  if (!is.character(x)) {
    x = as.character(x)
  }
  return(unserialize(charToRaw(x)))
}
