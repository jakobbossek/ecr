#' @title
#'   Wrap individuals in a 'setOfIndividuals'.
#'
#' @param individuals [\code{matrix}]\cr
#'   Matrix of individuals.
#' @param fitness [\code{matrix}]\cr
#'   Matrix of fitness values for the individuals.
#' @return [\code{setOfIndividuals}]
makePopulation = function(individuals, fitness = NULL) {
  makeS3Obj(
    individuals = individuals,
    fitness = fitness,
    classes = c("ecrPopulation", "setOfIndividuals")
  )
}
