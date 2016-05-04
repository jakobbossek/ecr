#' @title
#' Enhance control object with evolutionary operators.
#'
#' @description
#' Define the toolbox of all the evolutionary operators you wish to operate on
#' your problem.
#'
#' @note
#' Keep in mind, that all of the provided operators need to be compatible
#' with the \dQuote{representation} stored in the \code{control} object.
#'
#' @template arg_control
#' @template arg_generator
#' @template arg_parent_selector
#' @template arg_survival_selector
#' @template arg_mutator
#' @template arg_recombinator
#' @return [\code{ecr_control}] Modified control object.
#' @export
setupEvolutionaryOperators = function(
  control,
  parent.selector = getDefaultEvolutionaryOperators(control$representation, "parent.selector"),
  survival.selector = getDefaultEvolutionaryOperators(control$representation, "survival.selector"),
  generator = getDefaultEvolutionaryOperators(control$representation, "generator"),
  mutator = getDefaultEvolutionaryOperators(control$representation, "mutator"),
  recombinator = getDefaultEvolutionaryOperators(control$representation, "recombinator")) {
  assertClass(control, "ecr_control")

  control = setupParentSelector(control, parent.selector)
  control = setupSurvivalSelector(control, survival.selector)
  control = setupGenerator(control, generator)
  control = setupMutator(control, mutator)
  control = setupRecombinator(control, recombinator)

  return (control)
}

#' @title
#' Collection of functions to set specific evolutionay operators.
#'
#' @description
#' This functions can be used to append/set specific evolutionary operators
#' to the control object. However, multiple operators can be set with one function
#' call via \code{\link{setupEvolutionaryOperators}}.
#'
#' @param control [\code{ecr_control}]\cr
#'   ECR control object generated via \code{\link{setupECRControl}}.
#' @param operator [\code{ecr_operator}]\cr
#'   The matching ecr operator, e.g., an object of type \code{ecr_mutator} for
#'   \code{\link{setupMutator}}.
#' @rdname evolutionary_setters
#' @export
setupParentSelector = function(control, operator) {
  setupOperator(control, operator, "ecr_selector", "Parent selector", "parent.selector")
}

#' @rdname evolutionary_setters
#' @export
setupSurvivalSelector = function(control, operator) {
  setupOperator(control, operator, "ecr_selector", "Survival selector", "survival.selector")
}

#' @rdname evolutionary_setters
#' @export
setupGenerator = function(control, operator) {
  setupOperator(control, operator, "ecr_generator", "Generator", "generator")
}

#' @rdname evolutionary_setters
#' @export
setupMutator = function(control, operator) {
  setupOperator(control, operator, "ecr_mutator", "Mutator", "mutator")
}

#' @rdname evolutionary_setters
#' @export
setupRecombinator = function(control, operator) {
  setupOperator(control, operator, "ecr_recombinator", "Recombinator", "recombinator")
}

# Helper function to set operator internally.
#
# @param control [ecr_control]
#   ECR control object.
# @param operator [ecr_operator]
#   The corresponding operator to set.
# @param type [character(1)]
#   The expected type of the operator.
# @param description [character(1)]
#   Short string description of the operator.
# @param field [character(1)]
#   Name of the field in the control object where to store the operator.
# @return [ecr_control]
setupOperator = function(control, operator, type, description, field) {
  assertClass(control, "ecr_control")
  checkCorrectOperatorType(operator, type, description)
  checkOperatorIsCompatible(operator, control$representation)
  control[[field]] = operator
  return(control)
}

# @title
# Check if given operator is of the specified type.
#
# @param operator [ecr_operator]
#   Operator.
# @param class [character(1)]
#   Class.
# @param type [character(1)]
#   Type of the operator.
# @return Nothing
checkCorrectOperatorType = function(operator, class, type) {
  if (!inherits(operator, class)) {
    stopf("%s must be of class '%s', not '%s'.", type, class, collapse(attr(operator, "class"), sep = ", "))
  }
}

# @title
# Check whether an operator can handle a specific representation.
#
# @param operator [ecr_operator]
#   Operator.
# @param representation [character(1)]
#   Representation, i.e., float, binary, permutation or custom.
# @return [logical(1)]
checkOperatorIsCompatible = function(operator, representation) {
  if (!is.supported(operator, representation)) {
    stopf("Operator '%s' is not compatible with representation '%s'",
      getOperatorName(operator), representation
    )
  }
}

# @title
# Helper function which returns the defaults evolutionary operators for the
# standard representations.
#
# @param representation [\code{character(1)}]\cr
#   Genotype representation of the parameters. Available are binary, real,
#   permutation and custom.
# @param type [\code{character(1)}]\cr
#   Type of evolutionary operator. Possible are parent.selector, generator,
#   mutator, recombinator and survival.selector.
# @return [\code{ecr_operator}]
getDefaultEvolutionaryOperators = function(representation, type) {
  defaults = list(
    "float" = list(
      "parent.selector" = setupTournamentSelector(k = 2L),
      "generator" = setupUniformGenerator(),
      "mutator" = setupGaussMutator(),
      "recombinator" = setupIntermediateRecombinator(),
      "survival.selector" = setupGreedySelector()
    ),
    "binary" = list(
      "parent.selector" = setupTournamentSelector(k = 2L),
      "generator" = setupBinaryGenerator(),
      "mutator" = setupBitFlipMutator(),
      "recombinator" = setupCrossoverRecombinator(),
      "survival.selector" = setupGreedySelector()
    ),
    "permutation" = list(
      "parent.selector" = setupTournamentSelector(k = 2L),
      "generator" = setupPermutationGenerator(),
      "mutator" = setupSwapMutator(),
      "recombinator" = setupPMXRecombinator(),
      "survival.selector" = setupGreedySelector()
    )
  )

  if (representation %in% names(defaults)) {
    return(defaults[[representation]][[type]])
  }
  stopf("No defaults availiable for custom representation. You need to specify all
    operators by hand.")
}
