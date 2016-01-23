# @title
# Generate event dispatcher.
#
# @description
# The event dispatcher is a simple container for a hashmap of (event, actionForEvent)
# lists. The dispatcher has two important function properties, namely registerAction and
# fireEvent. The first one serves to fill the hashmap, i.e., to link actions to
# events. The latter triggers an event and calls all actions associated with this
# event.
#
# @return [\code{ecr_event_dispatcher}]
# List with the following components:
# \describe{
#   \item{registerAction}{Function to register a function 'fun' to an 'event.name'.}
#   \item{fireEvent}{Call all actions registered for 'event.name' and pass 'opt.state'
#   \item{getActionList}{Returns the hashmap.}
#   and '...' to them.}
#
# }
setupEventDispatcher = function(name) {
  action.list = list()

  # register action for event
  registerAction = function(event.name, fun) {
    assertChoice(event.name, choices = getAvailableEventNames())
    assertFunction(fun, args = c("opt.state", "..."), ordered = TRUE)
    if (is.null(action.list[[event.name]])) {
      action.list[[event.name]] = list()
    }
    action.list[[event.name]] <<- c(action.list[[event.name]], fun)
    invisible()
  }

  # call all actions for event
  fireEvent = function(event.name, opt.state, ...) {
    if (!is.null(action.list[[event.name]])) {
      for (action in action.list[[event.name]]) {
        action(opt.state, ...)
      }
    }
    invisible()
  }

  getActionList = function() {
    return(action.list)
  }

  makeS3Obj(
    registerAction = registerAction,
    fireEvent = fireEvent,
    getActionList = getActionList,
    classes = "ecr_event_dispatcher"
  )
}

#' Receive a character vector of all valid event names.
#' @export
getAvailableEventNames = function() {
  return(c(
    "onEAInitialized",
    "onMatingPoolGenerated",
    "onOffspringGenerated",
    "onPopulationUpdated",
    "onEAFinished"
  ))
}

fireEvent = function(event.name, control, opt.state, ...) {
  control$event.dispatcher$fireEvent(event.name, opt.state, ...)
}

#' @title
#' Register an action.
#'
#' @description
#' Register an action for a specific event in the EA circle.
#' See \code{\link{getAvailableEventNames}} for a list of available event names.
#'
#' @param control [\code{ecr_control}]\cr
#'   Control object.
#' @param event.name [\code{character(1)}]\cr
#'   Name of the event.
#' @param fun [\code{function()}]\cr
#'   Function with arguments \code{opt.state} and \code{...}.
#' @return [\code{ecr_control}] Updated control object.
#' @seealso \code{\link{getAvailableEventNames}}
#' @export
registerAction = function(control, event.name, fun) {
  assertClass(control, "ecr_control")
  control$event.dispatcher$registerAction(event.name, fun)
  return(control)
}
