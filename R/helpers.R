#' Generates ParamSet object out of a given soobench function.
#'
#' @param fn [\code{\link[soobench]{soo_function}}]\cr
#'   Source function.
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#'   Parameter set of type \code{\link[ParamHelpers]{ParamSet}}
#' @examples
#' library(soobench)
#' fn = ackley_function(8)
#' par.set = extractParamSetFromSOOFunction(fn)
#' print(par.set)
#' @export
extractParamSetFromSOOFunction = function(fn) {
  if (!is_soo_function(fn)) {
    stop("Function must be of type soo_function from soobench package.")
  }
  makeNumericParamSet(
    len = number_of_parameters(fn),
    id = "x",
    lower = lower_bounds(fn),
    upper = upper_bounds(fn),
    vector = FALSE
  )
}
