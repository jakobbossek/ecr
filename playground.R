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


generateRandomInitialPopulation = function(size, n.params, lower.bounds, upper.bounds) {
  design = matrix(0, nrow = size, ncol = n.params)
  for (i in seq(n.params)) {
    design[, i] = runif(size, min = lower.bounds[i], max = upper.bounds[i])
  }
  return(design)
}

getBestIndividual = function(population, population.fitness) {
  best.idx = which.min(population.fitness)
  best.fitness = population.fitness[best.idx]
  best.individual = population[best.idx, ]
  return(structure(
    list(fitness = best.fitness, individual = best.individual),
    class = "esooIndividual"))
}

computeFitness = function(individuals, fitness.fun) {
  apply(individuals, 1, fitness.fun)
}

terminiationCriterionFullfilled = function(current.iter, max.iter) {
  current.iter > max.iter
}

parentSelection = function(individuals, fitness, number.of.parents, strategy = "best") {
  idx = order(fitness)[seq(number.of.parents)]
  return(individuals[idx, , drop = FALSE])
}

recombinate = function(individuals, type = "intermediate", params = list(weight = 0.5)) {
  #FIXME: weight not considered until now
  child = apply(individuals, 2, sum) / 2
  return(t(as.matrix(child)))
}


# Mutation operators for real-valued vectors
gaussMutation = function(individuals, prob = 0.1) {
  n.params = ncol(individuals)
  n = nrow(individuals)
  for (i in seq(n)) {
    mutation.bool = (runif(n.params) <= 0.1)
    mutation = ifelse(mutation.bool, rnorm(1, mean = 0, sd = 0.1), 0)
    # catf("Mutation")
    # print(mutation)
    individuals[i, ] = individuals[i, ] + mutation
  }
  return(individuals)
}

survivalSelection = function(parents, children, parents.fitness, children.fitness, strategy = "mupluslambda") {
  source.population = rbind(parents, children)
  source.fitness = c(parents.fitness, children.fitness)
  size = nrow(parents)
  to.survive = order(source.fitness)[seq(size)]
  return(list(
    population = source.population[to.survive, ],
    fitness = source.fitness[to.survive]
    ))
}

correctBounds = function(individuals, lower.bounds, upper.bounds) {
  for (i in 1:nrow(individuals)) {
    for (j in 1:ncol(individuals)) {
      if (individuals[i, j] < lower.bounds[j]) {
        individuals[i, j] = lower.bounds[j]
      } else if (individuals[i, j] > upper.bounds[j]) {
        individuals[i, j] = upper.bounds[j]
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
  print(population)
  population.fitness = computeFitness(population, f)
  best = getBestIndividual(population, population.fitness)
  trace = makeTrace(n)
  trace = addToTrace(trace, best, 0)

  catf("Fitness of start population:")
  print(population.fitness)
  i = 1L
  while (!terminiationCriterionFullfilled(i, max.iter)) {
    catf("Beginning iteration %i", i)
    parents = parentSelection(population, population.fitness, number.of.parents = 2, strategy = "best")
    catf("Selected parents:")
    print(parents)
    #FIXME: how to add crossover params
    #FIXME: until now only one child generated
    children = recombinate(parents, type = "intermediate")
    children = gaussMutation(children)
    children = correctBounds(children, lower_bounds(f), upper_bounds(f))

    catf("Generated children:")
    children.fitness = computeFitness(children, f)
    tmp = cbind(as.data.frame(children), data.frame(fitness = children.fitness))
    print(tmp)

    # FIXME: elitism, survival of the fittest if (mu, lambda) strategy is used
    res = survivalSelection(population, children, population.fitness, children.fitness, strategy = "mupluslambda")
    population = res$population
    population.fitness = res$fitness

    best = getBestIndividual(population, population.fitness)
    trace = addToTrace(trace, best, i)

    i = i + 1
    #if (i == 3)
      #stopf("debug")
  }
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

res = esoo(sphere_fun, 50, 200)
plotTrace(res$trace)


