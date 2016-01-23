#' @title
#' Simple no output function.
#'
#' @description
#' No monitoring at all. Using this monitor practically deactivates monitoring.
#'
#' @return [\code{ecr_monitor}]
#' @export
makeNullMonitor = function() {
  makeMonitor(
    before = function(opt.state, ...) {},
    step = function(opt.state, ...) {},
    after = function(opt.state, ...) {}
  )
}
