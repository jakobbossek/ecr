library(soobench)
library(JBmisc)
library(BBmisc)
library(ggplot2)

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
    setOfIndividual s$population[i, ] = setOfIndividuals$population[i, ] + mutation
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


