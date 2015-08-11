library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

set.seed(1)
load_all(".")

# THIS FILE IS WORK-IN-PROGRESS/EXPERIMENAL

#FIXME: we need to check in setupECRControl or in ecr if the operators can work
# on multi-objective stuff
makeHypervolumeSelector = function() {
  makeSelector(
    selector = function(population, storage, n.select, control) {
      fitness = population$fitness
      population = population$individuals

      # do non-dominated sorting
      nds.res = doNondominatedSorting(fitness)
      ranks = nds.res$ranks
      idx.max = which(ranks == max(ranks))

      # there is exactly one individual that is "maximally" dominated
      if (length(idx.max) == 1L) {
        return(makePopulation(population[-idx.max], fitness[, -idx.max, drop = FALSE]))
      }

      # compute exclusive hypervolume contributions and remove the one with the smallest
      hvctrbs = computeHypervolumeContribution(fitness[, idx.max, drop = FALSE], ref.point = control$ref.point)
      die.idx = idx.max[getMinIndex(hvctrbs)]

      return(makePopulation(population[-die.idx], fitness[, -die.idx, drop = FALSE]))
    },
    supported.objectives = "multi-objective",
    name = "Hypervolume contribution selector",
    description = "description"
  )
}

ctrl = setupECRControl(
  n.population = 25L,
  n.offspring = 1L,
  representation = "float",
  monitor = makeConsoleMonitor(),
  stopping.conditions = setupStoppingConditions(max.iter = 150L)
)

ctrl = setupEvolutionaryOperators(
  ctrl,
  parent.selector = makeSimpleSelector(),
  mutator = makeGaussMutator(),
  recombinator = makeCrossoverRecombinator(),
  survival.selector = makeHypervolumeSelector()
)

ctrl$ref.point = c(11, 11)

obj.fn = smoof::makeZDT1Function(2L)

res = doTheEvolution(obj.fn, ctrl)

pl = visualizeParetoOptimalFront(obj.fn)
pf = as.data.frame(res$pareto.front)
pl = pl + geom_point(data = pf, aes(x = y1, y = y2), colour = "green")
stop("FIN")
