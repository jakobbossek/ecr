# Helper function for building up an optimization trace used by the EA
# to store the history of optimization.
#
# @param n.params [\code{integer(1)}]\cr
#   Number of dimensions of target function.
# @param param.names [\code{character}]\cr
#   Vector of names for the dimensions of the target function. Must be of length \code{n.params}.
# @param target.name [\code{character(1)}]\cr
#   Name of the target variable (we can have only one, because multi-objective optimization is not
#   supported).
makeTrace = function(n.params, param.names, target.name) {
  # this stuff sucks
  if (missing(param.names)) {
    param.names = paste("x", seq_len(n.params), sep = "")
  }
  if (missing(target.name)) {
    target.name = "y"
  }
  col.names = c(param.names, target.name, "generation")
  #FIXME: trace is empty data.frame after intialization
  #FIXME: saving param.names and target.name here is not a good idea
  #FIXME: ParamHelpers::makeOptPathDf does exactly what I need, but it is designed for
  #       the use of the different ParamHelpers parameter types I think.
  return(structure(
    list(
      trace = data.frame(),
      col.names = col.names,
      param.names = param.names,
      target.name = target.name,
      size = 1L
      ),
    class = "esooTrace"))
}


addToTrace = function(trace, individual, generation) {
  tmp = c(individual$individual, individual$fitness, generation)
  tmp = as.data.frame(t(tmp))
  colnames(tmp) = trace$col.names
  trace$trace = rbind(trace$trace, tmp)
  return(trace)
}
