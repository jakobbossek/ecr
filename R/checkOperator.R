# Checks operator attributes for validity
#
# @param operator
#   Operator to check.
checkOperator = function(operator) {
  checkArg(operator, "ecr_operator")
  checkArg(attr(operator$name), "character", len = 1L, na.ok = FALSE)
  checkArg(attr(operator$description), "character", len = 1L, na.ok = FALSE)
  checkArg(attr(operator$supported), "character", min.len = 1L, na.ok = FALSE)
}

checkMutator = function(mutator) {
  checkOperator(mutator)
  checkArg(mutator, "ecr_mutator")
  checkArg(attr(mutator$defaults), "list", na.ok = FALSE)
}

checkRecombinator = function(recombinator) {
  checkOperator(recombinator)
  checkArg(recombinator, "ecr_recombinator")
  checkArg(attr(recombinator$n.parents), "integer", len = 1L, lower = 2L, na.ok = FALSE)
  checkArg(attr(recombinator$defaults), "list", na.ok = FALSE)
}
