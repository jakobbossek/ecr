# @title
# Check termination conditions.
#
# @description
# Helper function which checks whether some stopping conditions
# are met and returns a termination object.
#
# @param opt.state [\code{ecr_opt_state}]
#   Optimization state object.
# @param control [\code{ecr_control}]
#   Control object.
# @return [stopObject]
doTerminate = function(opt.state, control) {
  stopObject = list()
  stopping.conditions = control$stopping.conditions
  # if we have not specified any stopping conditions always return the empty object
  if (!length(stopping.conditions)) {
    return(stopObject)
  }

  # otherwise iterate over stopping conditions and check
  for (stopping.conditions in stopping.conditions) {
    shouldStop = stopping.conditions(opt.state = opt.state)
    if (shouldStop) {
      stopObject$name = attr(stopping.conditions, "name")
      stopObject$message = attr(stopping.conditions, "message")
      break
    }
  }
  return(stopObject)
}
