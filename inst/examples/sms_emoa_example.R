library(smoof)
library(ecr)

# THIS FILE IS WORK-IN-PROGRESS/EXPERIMENAL

# COLLECTION OF FIXME
#FIXME: integrate NondominatedSetSelector as ecr_selector
#FIXME: we need to check in setupECRControl or in ecr if the operators can work
# on multi-objective stuff
#

# Get set of dominated individuals.
#
# @param x [list]
#   Set of 2D fitness values.
# @param fn [function]
#   Fitness function.
# @return [list]
#FIXME: formulate this for matrices where each column describes one vector. However,
# this is just a naive imolementation for testing purpose. Will be replaced
getDominatedSet = function(x) {
  n = length(x)

  # initialize set of dominated individuals
  dom.set = integer()
  dom.nrs = integer(n)

  for (i in seq(n)) {
    for (j in seq(n)) {
      if (i == j) {
        next
      }
      if (all(x[[j]] <= x[[i]])) {
        dom.nrs[i] = dom.nrs[i] + 1L
      }
    }
  }
  return(list(
    dom.set = which(dom.nrs != 0L),
    dom.nrs = dom.nrs
  ))
}

makeNondominatedSetSelector = function() {
  makeSelector(
    selector = function(population, n.select, control) {
      inds = population$individuals
      n = length(inds)
      #FIXME: ugly as sin. See fixme of getNondominatedSet
      fitness = as.list(as.data.frame(population$fitness))
      res = getDominatedSet(fitness)
      idx = getMaxIndex(res$dom.nrs)
      survive.idx = setdiff(seq(n), idx)
      return(makePopulation(inds[survive.idx], population$fitness[, survive.idx, drop = FALSE]))
    },
    supported.objectives = "multi-objective",
    name = "Nondominated set selector",
    description = "description"
  )
}

ctrl = setupECRControl(
  n.population = 20L,
  n.offspring = 10L,
  representation = "float",
  monitor = makeConsoleMonitor()
)

ctrl = setupEvolutionaryOperators(
  ctrl,
  # tournamend and roulette wheel work on scalar fitness values only
  # Thus, simply select two random elements for reproduction
  parent.selector = makeSimpleSelector(),
  mutator = makeGaussMutator(),
  recombinator = makeCrossoverRecombinator(),
  # everything can be used, we just needed a multi-objective-specific survival selector
  survivalSelector = makeNondominatedSetSelector()
)

obj.fn = smoof::makeZDT1Function(2L)

res = doTheEvolution(obj.fn, ctrl)

pl = smoof::visualizeParetoOptimalFront(obj.fn)
pf = res$pareto.front
pf = as.data.frame(t(pf))
names(pf) = paste0("x", 1:2)
pl = pl + geom_point(data = pf, aes(x = x1, y = x2), colour = "green")
print(pl)
stop("FIN")
