library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

set.seed(123)
load_all(".")

# THIS FILE IS WORK-IN-PROGRESS/EXPERIMENAL

# Implementation of surival selection operator of the NSGA-II algorithm.
#
# The NSGA-II merges the current population and the generated offspring and
# reduces it by means of the following procedure: It first applies the non
# dominated sorting algorithm to obtain the nondominated fronts. Starting with
# the first front, it fills the new population until the i-th front does not fit.
# It then applies the secondary crowding distance criterion to select the missing
# individuals from the i-th front.
makeNSGA2SurvivalSelector = function() {
  makeSelector(
    selector = function(population, n.select, control) {
      inds = population$individuals
      fitness2 = population$fitness
      #FIXME: f***! We need to transpose here. I should definitely decide myself
      # for column-wise or row-wise fitness. I guess the former is better suited
      # later for C(++)?
      fitness = t(fitness2)
      nondom.layers = doNondominatedSorting(fitness)

      # storage for indizes of selected individuals
      new.pop.idxs = integer()

      # get maximum rank, i.e., the number of domination layers
      max.rank = max(nondom.layers$ranks)

      # get the indizes of points for each domination layer
      idxs.by.rank = lapply(seq(max.rank), function(r) which(nondom.layers$ranks == r))

      # get the number of points in each domination layer ...
      front.len = sapply(idxs.by.rank, length)

      # ... cumulate the number of points of the domination layers ...
      cum.front.len = cumsum(front.len)

      # ... and determine the first domination layer, which does not fit as a whole
      front.first.nonfit = which.first(cum.front.len > n.select)

      if (front.first.nonfit > 1L) {
        # in this case at least one nondominated front can be added
        new.pop.idxs = unlist(idxs.by.rank[1:(front.first.nonfit - 1L)])
      }

      # how many points to select by second criterion, i.e., crowding distance?
      n.diff = n.select - length(new.pop.idxs)

      idxs.first.nonfit = idxs.by.rank[[front.first.nonfit]]
      cds = computeCrowdingDistance(fitness[idxs.first.nonfit, , drop = FALSE])
      idxs2 = order(cds, decreasing = TRUE)[1:n.diff]
      new.pop.idxs = c(new.pop.idxs, idxs.first.nonfit[idxs2])

      # merge the stuff and return
      return(makePopulation(inds[new.pop.idxs], fitness2[, new.pop.idxs, drop = FALSE]))
    },
    supported.objectives = "multi-objective",
    name = "NSGA-II survival selector",
    description = "nondominated sorting with potential crowding distance"
  )
}

# NSGA-II control object
ctrl = setupECRControl(
  n.population = 40L,
  n.offspring = 30L,
  representation = "float",
  monitor = makeConsoleMonitor(),
  stopping.conditions = setupStoppingConditions(max.iter = 100L)
)

ctrl = setupEvolutionaryOperators(
  ctrl,
  parent.selector = makeSimpleSelector(),
  mutator = makeGaussMutator(),
  recombinator = makeCrossoverRecombinator(),
  survival.selector = makeNSGA2SurvivalSelector()
)

obj.fn = smoof::makeZDT3Function(2L)

res = doTheEvolution(obj.fn, ctrl)

pl = visualizeParetoOptimalFront(obj.fn)
pf = as.data.frame(res$pareto.front)
pl = pl + geom_point(data = pf, aes(x = y1, y = y2), colour = "green")
print(pl)
