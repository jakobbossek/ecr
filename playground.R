library(soobench)
library(helpr)
library(BBmisc)

generateRandomInitialPopulation = function(size, n.params, lower.bounds, upper.bounds) {
  design = matrix(0, nrow = size, ncol = n.params)
  for (i in seq(n.params)) {
    design[, i] = runif(size, min = lower.bounds[i], max = upper.bounds[i])
  }
  return(design)
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
  population = generateRandomInitialPopulation(mu, number_of_parameters(f), lower_bounds(f), upper_bounds(f))
  catf("Initial Population generated.")
  print(population)
  population.fitness = computeFitness(population, f)
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

    i = i + 1
    #if (i == 3)
      #stopf("debug")
  }
  best.idx = which.min(population.fitness)
  best.fitness = population.fitness[best.idx]
  best.individual = population[best.idx, ]
  return(
    structure(list(
      best.param = best.individual,
      best.fitness = best.fitness
      ), class = "esoor")
  )
}

sphere_fun = generate_sphere_function(2)
res = esoo(sphere_fun, 50, 50)


