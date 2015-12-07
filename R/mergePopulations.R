#' @title
#' Merge populations.
#'
#' @description
#' Make one population out of multiple.
#'
#' @param ... [\code{list}]\cr
#'  List of objects of type \code{setOfIndividuals}.
#' @return [\code{setOfIndividuals}]
mergePopulations = function(...) {
  populations = list(...)

  individuals = do.call(c, extractSubList(populations, "individuals", simplify = FALSE))
  fitness = do.call(cbind, extractSubList(populations, "fitness", simplify = FALSE))

  makePopulation(
    individuals = individuals,
    fitness = fitness
  )
}
