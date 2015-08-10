library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

set.seed(123)
load_all(".")

# THIS FILE IS WORK-IN-PROGRESS/EXPERIMENAL

#FIXME: add doASEMOA function (wrapper for all that stuff here with all the
# paraemter the AS-EMOA expects)
#FIXME: fitness value in multi-objective case all need to be stored column-wise

# This is the main selection mechanism of the AS-EMOA.
# Remove the point which leads to highest
deltaOneUpdate = function(set, aspiration.set) {
  # here we need to apply this strange information. See the reference for details
  # yeah, I could use range here but it is more readable this way
  min1 = min(aspiration.set[, 1L])
  min2 = min(aspiration.set[, 2L])
  max1 = max(aspiration.set[, 1L])
  max2 = max(aspiration.set[, 2L])

  # transform
  set[, 1L] = (set[, 1L] - min1) / (max2 - min2)
  set[, 2L] = (set[, 2L] - min2) / (max1 - min1)

  return(computeAverageHausdorffDistance(set, aspiration.set))
}

# Implementation of surival selection operator of the AS-EMOA algorithm.
#
# The AS-EMAO selects the individuals which have the lowest average Hausdorff
# distance to a given aspiration set.
#
# @references Rudolph, G., Schütze, S., Grimme, C., Trautmann, H:
# An Aspiration Set EMOA Based on Averaged Hausdorff Distances.
# LION 2014: 153-156
makeASEMOASurvivalSelector = function() {
  makeSelector(
    selector = function(population, storage, n.select, control) {
      fitness = t(population$fitness)
      population = population$individuals
      # filter nondominated points
      nondom.idx = which.nondominated(fitness)
      population = population[nondom.idx]
      fitness = fitness[nondom.idx, ]

      n.archive = control$n.archive
      # if maximal number of individuals is not exceeded yet
      # simply return
      if (length(population) <= n.archive) {
        #catf("Still not enough individuals! Adding ...")
        return(makePopulation(population, t(fitness)))
      }

      # Otherwise we need to do the computationally more expensive part
      hausdorffDistances = lapply(seq(length(population)), function(idx) {
        deltaOneUpdate(fitness[-idx, ], control$aspiration.set)
      })

      # FIXME: here we need to check if there are multiple elements with this distance
      tmp = which.min(hausdorffDistances)

      #FIXME: again, all these transposing sucks. One column per objective vector will solve
      # this frequently occuring transformation
      return(makePopulation(population[-tmp], t(fitness[-tmp, ])))
    },
    supported.objectives = "multi-objective",
    name = "AS-EMOA selector",
    description = "Selection takes place based on (modified) average Hausdorff metric"
  )
}


obj.fn = smoof::makeZDT3Function(2L)

makeASEMOAGenerator = function() {
  generate = function(size, control) {
    uniformGenerator = makeUniformGenerator()
    population = uniformGenerator(size, control)
    #FIXME: here we need the objective function to compute the fitness values
    fitness = computeFitness(population, obj.fn)
    # now filter out dominated solutions
    #FIXME: transposing sucks! We need to save that stuff always one column per objective vector
    nondom.idx = which.nondominated(t(fitness))
    population$individuals = population$individuals[nondom.idx]
    return(population)
  }
  makeGenerator(generate,
    name = "AS-EMOA generator",
    description = "Generates uniformaly and reduces to non-dominated set",
    supported = "float"
  )
}

# NSGA-II control object
ctrl = setupECRControl(
  n.population = 100L,
  n.offspring = 1L,
  representation = "float",
  monitor = makeConsoleMonitor(),
  stopping.conditions = setupStoppingConditions(max.iter = 200L)
)

ctrl = setupEvolutionaryOperators(
  ctrl,
  generator = makeASEMOAGenerator(),
  parent.selector = makeSimpleSelector(),
  mutator = makeGaussMutator(),
  recombinator = makeCrossoverRecombinator(),
  survival.selector = makeASEMOASurvivalSelector()
)

#FIXME: maybe add the possibility to add further stuff to the control object
# without error-checking. Maybe the easiest way would be to use the ... argument
# of doTheEvolution. Simply append list(...) to the internal control object. Just
# check if there are no name clashes
ctrl$n.archive = 15L
ctrl$aspiration.set = matrix(
  c(0.2, 0.25,
    0.21, 0.2,
    0.18, 0.4), ncol = 2L, byrow = TRUE
)

res = doTheEvolution(obj.fn, ctrl)

pl = visualizeParetoOptimalFront(obj.fn)
pf = as.data.frame(res$pareto.front)
pl = pl + geom_point(data = pf, aes(x = y1, y = y2), colour = "green")
print(pl)
