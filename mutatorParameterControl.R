# Testing a mechanism which facitilates the handling of evolutionary parameters.
# Each evolutionary operator has its own defaults and his owen corresponding
# parameter validation function, which is called only one time during the
# creation of the ecr control object.

library(BBmisc)

# Operators like mutators, recombinator have a simple interface. Let's start with mutators, i. e.,
# mutation operators. They expect a setOfIndividuals and a operator.control parameter, the latter
# being a list of evolutionary parameters which the mutator needs to work properly. The default
# operator.control list is saved as an attribute 'defaults'. Moreover each mutator 'xyMutator' must
# have a corresponding 'xyMutatorCheck' function, which expects an operator.control list only and
# sanity checks them.

# The control object now works as follows:
# - firstly it checks if the given mutator is of the type ecr_mutator/ecr_operator
# - the string name of the mutator is extracted via deparse(substitute(mutatorFun))
# - the check arguments function is generated via paste(...)
# - the check arguments function is called. This ensures that before starting the
#   optimization process all parameters are checked! The alternative is to check this in
#   the mutator, but this is kind inefficient, since this check would be repeated each time the mutator
#   is called!

# define the mutator
xyMutationOperator = function(setOfIndividuals, operator.control = list(the.sd = 0.01)) {
  population = setOfIndividuals$population
  mutated.population = population +
    rnorm(nrow(population)*ncol(population), mean = 0, sd = operator.control$the.sd)
  makePopulation(individuals = mutated.population)
}

# define all the attributes for the mutator
attr(xyMutationOperator, "name") = "Gauss Mutator"
attr(xyMutationOperator, "description") = "Adds a low normally distributed noice to each allele."
attr(xyMutationOperator, "supportedRepresentations") = c("float")
attr(xyMutationOperator, "class") = c("ecr_operator", "ecr_mutator")

# define the default parameters for the mutator
attr(xyMutationOperator, "defaults") = list(the.sd = 0.001)

# define the check arguments function for the mutator
xyMutationOperatorCheck = function(operator.control) {
  catf("Checking evolutionary parameters for mutator %s", attr(xyMutationOperator, "name"))
  checkArg(operator.control$the.sd, cl = "numeric", len = 1L, lower = 0L)
}

# ecr control now only needs to call the corresponding function to validate the parameters.
ecr.control = function(control = list(), mutator) {
  operator.control = insert(control, attr(mutator, "defaults"))
  print(operator.control)
  mutator.fun.name = deparse(substitute(mutator))
  print(mutator.fun.name)
  mutator.checkfun.name = paste(mutator.fun.name, "Check", sep = "")
  print(mutator.checkfun.name)
  do.call(mutator.checkfun.name, list(operator.control = operator.control))
}

# Test
ecr.control(list(the.sd = "hallo"), xyMutationOperator)



