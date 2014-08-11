# Checks whether the EA run should terminate.
#
# @param current.iter [\code{integer(1)}]\cr
#   Current iteration of the algorithm.
# @param max.iter [\code{integer(1)}]\cr
#   Maximum number of iterations.
# @param global.optimum.param [\code{numeric}]\cr
#   Parameter values of global optimum or NA.
# @param best [\code{ecrIndividual}]\cr
#   Current best individual.
# @param termination.eps [\code{numeric(1)}]\cr
#   Maximal gap between best individual so far and the global optimum.
# @param start.time [\code{POSIXct}]\cr
#   Start time as returned by \code{Sys.time()}.
# @param max.time [\code{integer}]\cr
#   Maximal time budget in seconds.
# @return [\code{integer(1)}] Return code. See getTerminationMessage for encoding.
getTerminationCode = function(
	current.iter, max.iter,
	global.optimum.param, best, termination.eps,
	start.time, max.time) {
	if (didReachMaximumIterations(current.iter, max.iter)) {
		return(0L)
	}
  	if (didReachToleranceLevel(global.optimum.param, best, termination.eps)) {
    	return(1L)
    }	
    if (didReachMaximumTimeBudget(start.time, max.time)) {
    	return(2L)
    }
  	return(-1L)
}

getTerminationMessage = function(termination.code) {
	termination.messages = c(
		"Reached maximal number of iterations.",
		"Reached tolerence level.",
		"Time budget exceeded."
	)
	if (termination.code %nin% 0:2) {
		stopf("Unknown terminaton code %i.", termination.code)
	}
	return(termination.messages[termination.code + 1])
}

didReachMaximumIterations = function(current.iter, max.iter) {
	return(current.iter > max.iter)
}

didReachToleranceLevel = function(global.optimum.param, best, termination.eps) {
	if (is.null(global.optimum.param)) {
		return(FALSE)
	}
    best.param = best$individual
    #FIXME: this breaks on mixed functions!
    gap = sqrt(sum((best.param - as.numeric(global.optimum.param))^2))
    return(gap < termination.eps)
}

didReachMaximumTimeBudget = function(start.time, max.time) {
	current.time = Sys.time()
    time.difference.in.seconds = difftime(current.time, start.time, units = "secs")
    return(time.difference.in.seconds >= max.time)
}