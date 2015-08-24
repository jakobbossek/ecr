#' @title
#'   Enhance control object with evolutionary operators.
#'
#' @description
#'   Define the toolbox of all the evolutionary operators you wish to operate on
#'   your problem.
#'
#' @note
#'   Keep in mind, that all of the provided operators need to be compatible
#'   with the \dQuote{representation} stored in the \code{control} object.
#'
#' @param control [\code{ecr_control}]\cr
#'   ECR control object generated via \code{\link{setupECRControl}}.
#' @param generator [\code{ecr_generator}]\cr
#'   Generator operator of type \code{ecr_generator} for the generation of the initial
#'   population.
#' @param parent.selector [\code{ecr_selector}]\cr
#'   Selection operator which implements a procedure to copy individuals from a
#'   given population to the mating pool, i. e., allow them to become parents.
#' @param survival.selector [\code{ecr_selector}]\cr
#'   Selection operator which implements a procedurce to extract individuals from
#'   a given set, which should survive and set up the next generation.
#' @param mutator [\code{ecr_mutator}]\cr
#'   Mutation operator of type \code{ecr_mutator}.
#' @param recombinator [\code{ecr_recombinator}]\cr
#'   Recombination operator of type \code{ecr_recombinator}.
#' @param mutator.control [\code{list}]\cr
#'   List of evolutionary parameters for the corresponding mutation operator. See the
#'   help pages for the mutation operators for the needed values.
#' @param recombinator.control [\code{list}]\cr
#'   List of evolutionary parameters for the corresponding recombination operator. See the
#'   help pages for the recombination operators for the needed values.
#' @return [\code{ecr_control}] Modified control object.
#' @export
setupEvolutionaryOperators = function(
  control,
  parent.selector = getDefaultEvolutionaryOperators(control$representation, "parent.selector"),
  survival.selector = getDefaultEvolutionaryOperators(control$representation, "survival.selector"),
  generator = getDefaultEvolutionaryOperators(control$representation, "generator"),
  mutator = getDefaultEvolutionaryOperators(control$representation, "mutator"),
  recombinator = getDefaultEvolutionaryOperators(control$representation, "recombinator"),
  mutator.control = list(),
  recombinator.control = list()) {
  assertClass(control, "ecr_control")
  representation = control$representation

  assertClass(mutator, "ecr_mutator")
  assertList(mutator.control, any.missing = FALSE)
  assertList(recombinator.control, any.missing = FALSE)

  # check passed selector(s)
  checkCorrectOperatorType(parent.selector, "ecr_selector", "Parent selector")
  checkCorrectOperatorType(survival.selector, "ecr_selector", "Survival selector")

  # Check passed mutator
  checkCorrectOperatorType(mutator, "ecr_mutator", "Mutator")
  checkMutator(mutator)
  mutator.control = prepareOperatorParameters(mutator, mutator.control)

  # Check arguments of recombinator
  checkCorrectOperatorType(recombinator, "ecr_recombinator", "Recombinator")
  checkRecombinator(recombinator)
  recombinator.control = prepareOperatorParameters(recombinator, recombinator.control)

  # check generator
  checkCorrectOperatorType(generator, "ecr_generator", "Generator")

  sapply(c(generator, mutator, recombinator), function(operator) {
    if (!is.supported(operator, representation)) {
      stopf("Operator '%s' is not compatible with representation '%s'",
        getOperatorName(operator), representation
      )
    }
  })

  # store stuff in control object
  control$parent.selector = parent.selector
  control$survival.selector = survival.selector
  control$generator = generator
  control$mutator = mutator
  control$recombinator = recombinator
  control$mutator.control = mutator.control
  control$recombinator.control = recombinator.control

  return (control)
}

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

# Helper function which constructs control object for a given operator
# and checks the user parameters for validity.
#
# @param operator [\code{ecr_operator}]\cr
#   Operator object.
# @param parameters [\code{list}]\cr
#   List of parameters provedided by the user for the operator.
# @return [\code{list}]
#   List of checked parameters.
prepareOperatorParameters = function(operator, input.params) {
  defaults = getOperatorDefaultParameters(operator)
  params = insert(defaults, input.params)
  params[setdiff(names(params), names(defaults))] = NULL
  do.call(getOperatorCheckFunction(operator), list(params))
  return(params)
}

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
      "parent.selector" = makeRouletteWheelSelector(),
      "generator" = makeUniformGenerator(),
      "mutator" = makeGaussMutator(),
      "recombinator" = makeIntermediateRecombinator(),
      "survival.selector" = makeGreedySelector()
    ),
    "binary" = list(
      "parent.selector" = makeRouletteWheelSelector(),
      "generator" = makeBinaryGenerator(),
      "mutator" = makeBitFlipMutator(),
      "recombinator" = makeCrossoverRecombinator(),
      "survival.selector" = makeGreedySelector()
    ),
    "permutation" = list(
      "parent.selector" = makeRouletteWheelSelector(),
      "generator" = makePermutationGenerator(),
      "mutator" = makeSwapMutator(),
      "recombinator" = makePMXRecombinator(),
      "survival.selector" = makeGreedySelector()
    )
  )

  if (representation %in% names(defaults)) {
    return(defaults[[representation]][[type]])
  }
  stopf("No defaults availiable for custom representation. You need to specify all
    operators by hand.")
}
