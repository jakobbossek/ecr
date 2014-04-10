#' Generates control object.
#'
#' The esoo package offers a framework for evolutionary computing and therefore offers
#' a lot of customization options. The control object is a simple but powerful
#' wrapper for all these options and sets convenient default options.
#'
#' @export
esoo.control = function(
  population.size,
  offspring.size,
  representation,
  n.targets = 1L,
  max.iter = 100L,
  show.info = TRUE) {
  #FIXME: add further options regarding the default operators, representations and so on
  #FIXME: Allow (1+1) EA or not?
  checkArg(population.size, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(offspring.size, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(representation, choices = getAvailableRepresentations())
  checkArg(n.targets, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(max.iter, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  #FIXME: add monitoring option and different monitoring funs
  checkArg(show.info, cl = "logical", len = 1L, na.ok = FALSE)

  structure(list(
    population.size = population.size,
    offspring.size = offspring.size,
    representation = representation,
    n.targets = n.targets,
    max.iter = max.iter,
    show.info = show.info), class = "esoo_control")
}
