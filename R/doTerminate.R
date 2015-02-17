# Helper function which checks whether some stopping conditions
# are met and returns a termination object.
#
# @param stopping.funs [list]
#   List of stopping conditions.
# @param envir [environment]
#   Environment in which to look for some maybe neccessary variables.
# @return [stopObject]
doTerminate = function(stopping.funs, envir = parent.frame()) {
    stopObject = list()

    # if we have not specified any stopping conditions always return the empty object
    if (!length(stopping.funs)) {
        return(stopObject)
    }

    # otherwise iterate over stopping conditions and check
    for (stopping.fun in stopping.funs) {
        shouldStop = stopping.fun(envir = envir)
        if (shouldStop) {
            stopObject$name = attr(stopping.fun, "name")
            stopObject$message = attr(stopping.fun, "message")
            break
        }
    }
    return(stopObject)
}
