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
      # this is a very ugly first implementation. Do this more R-like
      new.pop = list()
      new.fit = list()
      i = 1L
      n = 0L
      repeat {
        # get indizes of the points with rank i
        idxs = which(nondom.layers$ranks == i)
        # check if adding the whole i-th layer overshoots the population size
        n = n + length(idxs)
        if (n > n.select) {
          n = n - length(idxs)
          # debugging
          catf("Domination layer %i is to large. Stopping first criterion.", i)
          catf("Inds in new pop: %i, Inds needed: %i", n, n.select)
          break
        }
        # add front to new population
        new.pop[[i]] = inds[idxs]
        new.fit[[i]] = fitness2[, idxs, drop = FALSE]
        i = i + 1L
      }

      # check whether the second criterion needs to be applied
      if (n != n.select) {
        catf("Population size not reached. Applying crowding distance criterion.")
        # how many elements do we need to select by second criterion?
        n.diff = n.select - n
        # crowding distance for the i-th domination front
        idxs = which(nondom.layers$ranks == i)
        #print(idxs)
        cds = computeCrowdingDistance(fitness[idxs, , drop = FALSE])
        #print(cds)
        # sort the indizes in decreasing order (higher crowding distance is beneficial)
        idxs2 = order(cds, decreasing = TRUE)[1:n.diff]
        # fill in the remaining individuals
        new.pop[[i]] = inds[idxs[idxs2]]
        new.fit[[i]] = fitness2[, idxs[idxs2], drop = FALSE]
      }

      # merge the stuff and return
      new.pop = do.call(c, new.pop)
      new.fit = do.call(cbind, new.fit)
      return(makePopulation(new.pop, new.fit))
    },
    supported.objectives = "multi-objective",
    name = "NSGA-II survival selector",
    description = "nondominated sorting with potential crowding distance"
  )
}

# NSGA-II control object
ctrl = setupECRControl(
  n.population = 50L,
  n.offspring = 10L,
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
stop("FIN")
