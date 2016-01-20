#' @title
#' Generate a population.
#'
#' @description
#' Wrap individuals in a \code{setOfIndividuals}. This function is of particular
#' interest when one wants to write population generators for non-standard
#' representations.
#'
#' @param individuals [\code{matrix}]\cr
#'   List of individuals.
#' @param fitness [\code{matrix}]\cr
#'   Matrix with one row of fitness values for the individuals.
#'   Default is \code{NULL}.
#' @return [\code{setOfIndividuals}]
#' @export
makePopulation = function(individuals, fitness = NULL) {
  assertList(individuals, any.missing = FALSE)
  if (!testNull(fitness)) {
    checkMatrix(fitness, any.missing = FALSE, nrows = 2L, ncols = length(individuals))
  }

  makeS3Obj(
    individuals = individuals,
    fitness = fitness,
    classes = c("ecr_population", "setOfIndividuals")
  )
}
