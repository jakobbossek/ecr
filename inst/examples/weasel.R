library(ecr)

# Simple EA for R. Dawkin’s "METHINKS IT IS LIKE A WEASEL“ problem.
# See http://rosettacode.org/wiki/Evolutionary_algorithm for details.

# reproducibility
set.seed(1)

# optimal value. I.e., the target string.
target = unlist(strsplit("METHINKS IT IS LIKE A WEASEL", ""))

# character set the EA works on
char.set = c(LETTERS, " ")

# no smoof function since we use a custom representation here
# ("string functions" not a default genotype)
obj.fun = function(string, target) {
  sum(string != target) / length(target)
}

# here we want to minimize the hamming distance
task = makeOptimizationTask(obj.fun, n.objectives = 1L)

# helper function to create a mutator for strings, i.e., character vectors.
# It works by selecting genes, i.e., string positions, with a low probability p,
# and replacing these with random values from the char.set.
setupStringMutator = function(p) {
  force(p)
  makeMutator(
    name = "String mutator",
    mutator = function(ind, task, control) {
      # get char.set
      char.set = control$custom.constants$char.set
      # determine which genes to mutate
      idx = which(runif(length(ind)) < p)
      n.mut = length(idx)
      # perform mutation
      if (n.mut > 0)
        ind[idx] = sample(char.set, n.mut, replace = TRUE)
      return(ind)
    },
    description = "",
    supported = "custom" # non-standard representation
  )
}

# generator function. Creates an initial population of random strings.
stringGenerator = makeGenerator(
  generator = function(size, task, control) {
    # extract neccessary information
    char.set = control$custom.constants$char.set
    n = length(control$custom.constants$target)
    makePopulation(lapply(seq(size), function(i) {
      sample(char.set, length(target), replace = TRUE)
    }))
  },
  name = "String generator",
  description = "Generates random strings from a character set",
  supported = "custom"
)

control = setupECRControl(
  n.population = 1L, n.mating.pool = 1L, n.offspring = 100L,
  representation = "custom",
  # variables needed by some of the operators
  custom.constants = list(char.set = char.set, target = target),
  stopping.conditions = list(setupMaximumIterationsTerminator(100L))
)

control = setupEvolutionaryOperators(
  control,
  generator = stringGenerator,
  mutator = setupStringMutator(p = 0.05),
  recombinator = setupNullRecombinator(),
  survival.selector = setupGreedySelector(),
  parent.selector = setupGreedySelector()
)

res = doTheEvolution(task, control, more.args = list(target))

print("Best parameter found:")
print(paste(res$best.param, collapse = ""))
