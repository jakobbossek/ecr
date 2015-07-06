#' @title
#'   Generate population from individuals 
#' @description 
#'   Helper function for wrapping individuals in a 'setOfIndividuals'.
#'
#' @param individuals [\code{list}]\cr
#'   List of individuals.
#' @param fitness [\code{matrix}]\cr
#'   Matrix with one row of fitness values for the individuals.
#'   Default is \code{NULL}.
#' @return [\code{setOfIndividuals}]
#' @export
makePopulation = function(individuals, fitness = NULL) {
  assertList(individuals, any.missing = FALSE)
  if (!testNull(fitness))
    assertMatrix(fitness, any.missing = FALSE, nrows = 1, ncols = length(individuals))
  
  makeS3Obj(
    individuals = individuals,
    fitness = fitness,
    classes = c("ecrPopulation", "setOfIndividuals")
  )
}
