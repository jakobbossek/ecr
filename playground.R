library(soobench)
library(JBmisc)
library(BBmisc)
library(ggplot2)

# TODO
# - think about genral structure of function
# - think about what type of elements should be wrapped in S3 classes:
#   population (has size, individuals), individual (has fitness value, date of birth, param values)
#   optimization trace (data frame with best individuals of each iteration)
# - what types of traget funs should be supported? Just funs with numeric-only parameters like the
#   soobnech funs or allow discrete params as well?
# - check Bernds ParamHelpers package thorougly. Maybe makeOptPath, all the make...Param funs can
#   be used in this package.
# - research for common (and not so common) mutation, selection and recombination operators
# - investigate existing R packages for evolutionary optimization (RFreak, GA, ...)
# - think about the best programmatical interface solution for the user. It should be as simple as
#   possible to extend the functionality and write propriatary mutation, selection ... funs.
# - at all costs follow the "DRY" principle, i.e., "don't repeat yourself"
# - 

# IDEAS
# - allow chaining of mutators, i. e., allow to apply more than one mutator to the offspring

makeTrace = function(n.params, param.names, target.name) {
  # this stuff sucks
  if (missing(param.names)) {
    param.names = paste("x", seq_len(n.params), sep = "")
  }
  if (missing(target.name)) {
    target.name = "y"
  }
  col.names = c(param.names, target.name, "generation")
  return(structure(
    list(
      trace = data.frame(),
      col.names = col.names,
      param.names = param.names,
      target.name = target.name,
      size = 1L
      ),
    class = "esooTrace"))
}

addToTrace = function(trace, individual, generation) {
  tmp = c(individual$individual, individual$fitness, generation)
  tmp = as.data.frame(t(tmp))
  colnames(tmp) = trace$col.names
  trace$trace = rbind(trace$trace, tmp)
  return(trace)
}

makePopulation = function(individuals, fitness) {
  res = list(population = individuals)
  if (!missing(fitness))
    res$fitness = fitness
  structure(
    res,
    class = c("esooPopulation", "setOfIndividuals"))
}

mergePopulations = function(individuals1, individuals2) {
  makePopulation(
    individuals = rbind(individuals1$population, individuals2$population),
    fitness = c(individuals1$fitness, individuals2$fitness)
    )
}

generateRandomInitialPopulation = function(size, n.params, lower.bounds, upper.bounds) {
  population = matrix(0, nrow = size, ncol = n.params)
  for (i in seq(n.params)) {
    population[, i] = runif(size, min = lower.bounds[i], max = upper.bounds[i])
  }
  makePopulation(population)
}

getBestIndividual = function(setOfIndividuals) {
  stopifnot(inherits(setOfIndividuals, "setOfIndividuals"))
  population = setOfIndividuals$population
  population.fitness = setOfIndividuals$fitness
  best.idx = which.min(population.fitness)
  best.fitness = population.fitness[best.idx]
  best.individual = population[best.idx, ]
  return(structure(
    list(fitness = best.fitness, individual = best.individual),
    class = "esooIndividual"))
}

computeFitness = function(setOfIndividuals, fitness.fun) {
  stopifnot(inherits(setOfIndividuals, "setOfIndividuals"))
  fitness.values = apply(setOfIndividuals$population, 1, fitness.fun)
  setOfIndividuals$fitness = fitness.values
  return(setOfIndividuals)
}

isTerminiationCriterionFullfilled = function(current.iter, max.iter) {
  current.iter > max.iter
}

parentSelection = function(setOfIndividuals, number.of.parents, strategy = "best") {
  stopifnot(inherits(setOfIndividuals, "setOfIndividuals"))
  individuals = setOfIndividuals$population 
  fitness = setOfIndividuals$fitness
  to.keep = order(fitness)[seq(number.of.parents)]
  makePopulation(individuals = individuals[to.keep, ], fitness = fitness[to.keep])
}

recombinate = function(individuals, type = "intermediate", params = list(weight = 0.5)) {
  #FIXME: weight not considered until now

  child = apply(individuals$population, 2, sum) / 2
  makePopulation(t(as.matrix(child)))
}


# Mutation operators for real-valued vectors
gaussMutation = function(setOfIndividuals, prob = 0.1) {
  n.params = ncol(setOfIndividuals$population)
  n = nrow(setOfIndividuals$population)
  for (i in seq(n)) {
    mutation.bool = (runif(n.params) <= 0.1)
    mutation = ifelse(mutation.bool, rnorm(1, mean = 0, sd = 0.2), 0)
    setOfIndividuals$population[i, ] = setOfIndividuals$population[i, ] + mutation
  }
  return(setOfIndividuals)
}

selectForSurvival = function(setOfIndividuals, pop.size, strategy = "mupluslambda") {
  individuals = setOfIndividuals$population
  fitness = setOfIndividuals$fitness
  to.survive = order(fitness)[seq(pop.size)]
  makePopulation(
    individuals = individuals[to.survive, ],
    fitness = fitness[to.survive]
    )
}

correctBounds = function(individuals, lower.bounds, upper.bounds) {
  for (i in 1:nrow(individuals$population)) {
    for (j in 1:ncol(individuals$population)) {
      if (individuals$population[i, j] < lower.bounds[j]) {
        individuals$population[i, j] = lower.bounds[j]
      } else if (individuals$population[i, j] > upper.bounds[j]) {
        individuals$population[i, j] = upper.bounds[j]
      }
    }
  }
  return(individuals)
}

esoo = function(f, max.iter, mu) {
  # maybe make use of Bernds makeOptPathDf and ParamHelpers for this.

  n = number_of_parameters(f)
  population = generateRandomInitialPopulation(mu, n, lower_bounds(f), upper_bounds(f))
  catf("Initial Population generated.")
  population = computeFitness(population, f)
  best = getBestIndividual(population)
  trace = makeTrace(n)
  trace = addToTrace(trace, best, 0)

  i = 1L
  while (!isTerminiationCriterionFullfilled(i, max.iter)) {
    cat(".")
    parents = parentSelection(population, number.of.parents = 2, strategy = "best")
    #FIXME: how to add crossover params
    #FIXME: until now only one child generated
    children = recombinate(parents, type = "intermediate")
    children = gaussMutation(children)
    children = correctBounds(children, lower_bounds(f), upper_bounds(f))

    children = computeFitness(children, f)
    population = mergePopulations(population, children)

    # FIXME: elitism, survival of the fittest if (mu, lambda) strategy is used
    population = selectForSurvival(population, mu, strategy = "mupluslambda")

    best = getBestIndividual(population)
    trace = addToTrace(trace, best, i)

    i = i + 1
    #if (i == 3)
      #stopf("debug")
  }
  catf("\nEA finished!")
  return(
    structure(list(
      best.param = best$individual,
      best.fitness = best$fitness,
      trace = trace
      ), class = "esoor")
  )
}

plotTrace = function(trace) {
  trace = trace$trace
  pl = ggplot(data = trace, aes(x = generation, y = y))
  pl = pl + geom_line()
  pl = pl + ggtitle("Optimization trace.")
  pl = pl + xlab("Generation") + ylab("Best fitness value")
  print(pl)
}

sphere_fun = generate_sphere_function(2)

res = esoo(sphere_fun, 100, 50)
plotTrace(res$trace)


