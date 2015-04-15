#' Define the toolbix of all the evolutionary operators you wish to operate on
#' your problem.
#'
#' @note Keep in mind, that all of the provided operators need to be compatible
#' with the \dQuote{representation} stored in the \code{control} object.
#'
#' @param control [\code{ecr_control}]\cr
#'   ECR control object generated via \code{\link{setupECRControl}}.
#' @param selector [\code{ecr_selector}]\cr
#'   Selection operator which implements a procedure to copy individuals from a
#'   given population to the mating pool, i. e., allow them to become parents.
#' @param generator [\code{ecr_generator}]\cr
#'   Generator operator of type \code{ecr_generator} for the generation of the initial
#'   population.
#' @param mutator [\code{ecr_mutator}]\cr
#'   Mutation operator of type \code{ecr_mutator}.
#' @param mutationStrategyAdaptor [\code{function}]\cr
#'   This is an experimental parameter. Hence, you should be careful when using it.
#'   Serves to offer the possibility to adapt parameters of the mutation algorithm
#'   (e. g. mutation stepsize \eqn{\sigma} for Gaussian mutation) in each iteration.
#'   The function needs to expect the parameters \dQuote{operator.control} and
#'   \dQuote{opt.path}, the last being of type \code{\link[ParamHelpers]{OptPath}} and
#'   must return the modified \dQuote{operator.control} object. The default does
#'   nothing.
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
  selector = makeSimpleSelector(),
  generator = getDefaultEvolutionaryOperators(control$representation, "generator"),
  mutator = getDefaultEvolutionaryOperators(control$representation, "mutator"),
  #FIXME: this stuff is experimental.
  mutationStrategyAdaptor = function(operator.control, opt.path) {
    return(operator.control)
  },
  recombinator = getDefaultEvolutionaryOperators(control$representation, "recombinator"),
  mutator.control = list(),
  recombinator.control = list()) {
  assertClass(control, "ecr_control")
  representation = control$representation

  assertClass(mutator, "ecr_mutator")
  assertList(mutator.control, any.missing = FALSE)
  assertFunction(mutationStrategyAdaptor, args = c("operator.control", "opt.path"), ordered = TRUE)
  assertList(recombinator.control, any.missing = FALSE)

  # check passed selector
  checkCorrectOperatorType(selector, "ecr_selector", "Selector")

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
  control$selector = selector
  control$generator = generator
  control$mutator = mutator
  control$mutationStrategyAdaptor = mutationStrategyAdaptor
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

getDefaultEvolutionaryOperators = function(representation, type) {
  defaults = list(
    "float" = list(
      "generator" = makeUniformGenerator(),
      "mutator" = makeGaussMutator(),
      "recombinator" = makeIntermediateRecombinator()
    ),
    "binary" = list(
      "generator" = makeBinaryGenerator(),
      "mutator" = makeBitFlipMutator(),
      "recombinator" = makeCrossoverRecombinator()
    ),
    "permutation" = list(
      "generator" = makePermutationGenerator(),
      "mutator" = makeSwapMutator(),
      #FIXME: later add a good
      "recombinator" = makeNullRecombinator()
    )
  )

  if (representation %in% names(defaults)) {
    return(defaults[[representation]][[type]])
  }
  stopf("No defaults availiable for custom representation. You need to specify all
    operators by hand.")
}
