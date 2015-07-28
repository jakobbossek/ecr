#' @title
#'   Simple no output function.
#'
#' @description
#'   No monitoring at all. Using this monitor practically deactivates monitoring.
#'
#' @return [\code{ecr_monitor}]
#'
#' @export
makeNullMonitor = function() {
  makeMonitor(
    before = function(envir = parent.frame()) {},
    step = function(envir = parent.frame()) {},
    after = function(envir = parent.frame()) {}
  )
}
