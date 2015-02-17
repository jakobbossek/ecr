#' No monitoring at all.
#'
#' @return [\code{ecr_monitor}]
#' @export
makeNullMonitor = function() {
  makeMonitor(
    before = function(envir = parent.frame()) {},
    step = function(envir = parent.frame()) {},
    after = function(envir = parent.frame()) {}
  )
}
