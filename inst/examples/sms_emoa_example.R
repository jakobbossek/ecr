library(smoof)
library(ecr)

ctrl = setupECRControl(
  n.population = 10L,
  n.offspring = 1L,
  representation = "float",
  n.targets = 2L,
  monitor = makeConsoleMonitor()
)

ctrl = setupEvolutionaryOperators(
  ctrl,
  selector = makeTournamentSelector(),
  mutator = makeGaussMutator(),
  recombinator = makeCrossoverRecombinator(),
  survivalSelector = makeNondominatedSetSelector()
)

# Get set of dominated individuals.
#
# @param x [list]
#   Set of 2D fitness values.
# @param fn [function]
#   Fitness function.
# @return [list]
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

# Determine the number of elements for each individual by which it is dominated.
#
# @param x [list]
#   Set of fitness values.
# @return [integer]
#   Each position contains the number of individuals by which this one is dominated.
getDominanceNumber = function()
inds = list(c(1, 2), c(2, 1), c(2, 2), c(1, 3), c(1.5, 1.3))

print(getDominatedSet(inds))


obj.fn = smoof::makeZDT1Function(2L)

