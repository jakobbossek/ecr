# @title
#   Wrap individuals in a 'setOfIndividuals'.
#
# @param [\code{matrix}]\cr
#   Matrix of individuals.
# @param [\code{numeric}]\cr
#   Vector of fitness values for the individuals.
# @return [\code{setOfIndividuals}]
makePopulation = function(individuals, fitness = NULL) {
  makeS3Obj(
    individuals = individuals,
    fitness = fitness,
    classes = c("ecrPopulation", "setOfIndividuals")
  )
}
