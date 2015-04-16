# ecr: Evolutionary Computing in R

[![Build Status](https://travis-ci.org/jakobbossek/ecr.svg?branch=master)](https://travis-ci.org/jakobbossek/ecr)
[![Build status](https://ci.appveyor.com/api/projects/status/eu0nns2dsgocwntw/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/ecr/branch/master)
[![Coverage Status](https://coveralls.io/repos/jakobbossek/ecr/badge.svg)](https://coveralls.io/r/jakobbossek/ecr)

The **ecr** package provides a powerful framework for **Evolutionary Computation** in R. It has build-in support for several standard genotypes like floating-point numbers, binary strings and permutations. Beside different pre-defined building blocks in form of evolutionary operators (selectors, mutators, recombinators), e.g., k-Tournament-Selection, Gauss-Mutation, Crossover, ecr offers the possibility to extend the available toolbox by defining new operators. Moreover, if the predefined genomes are not sufficient for the problem at hand, a custom mode is available to customize even the genotype.

## Overview of features

* Optimization of single-objective functions
* Build-in genotypes *float*, *binary*, and *permutation*
* Big collection of evolutionary operators
* Storing of the optimization process in an optimization path
* Methods to visualize results

# Installation instructions

The package will be available in a first version at [CRAN](http://cran.r-project.org) soon. If you are interested in trying out and playing around with the current github developer version use the [devtools](https://github.com/hadley/devtools) package and type the following command in R.

```splus
devtools::install_github("jakobbossek/ecr")
```

## Short example

In this section we want to optimize a one dimensional function with an Evolutionary Algorithm using just the evolutionary operators shipped with the package. A more in-depth introduction will be made available soon.

The [smoof](https://github.com/jakobbossek/smoof) R package provides a collection of different single objective test functions commonly used in algorithm benchmarking. As an example we are going to search for the global optimum of the one-dimensional Rastrigin function.

```splus
library(smoof)
library(ecr)

obj.fun = makeRastriginFunction(dimensions = 1L)
plot(obj.fun)
```

As a next step we generate an ecr *control object*, which holds all the neccessary parameters for the evolutionary algorithm. The construction of this object needs consists of generating the object itself and kind of decorating it with some evolutionary operators.

For our setup we choose the *atural* representation with real-valued numbers as the genotype, a population size of 20 individuals with 5 individuals being created by recombination and mutation in each generation. Furthermore we decide to use a 'plus' survival strategy, i. e., the current population and the offspring will be merged before survival selection takes place. Gauss mutation with a standard deviance of 0.005 serves as the mutation operator and we keep the intermediate recombination operator (which is the default for representation float). Moreover we define a maximal number of 50 generations.

```splus
control = setupECRControl(
  n.population = 20L,
  n.offspring = 10L,
  representation = "float",
  survival.strategy = "plus",
  save.population.at = 0:50L,
  stopping.conditions = list(
    makeMaximumIterationsStoppingCondition(max.iter = 50L)
  )
)
control = setupEvolutionaryOperators(
  control,
  mutator = makeGaussMutator(0.005)
)
print(control)
```

Now lets start the optimization process and print the result object, which contains the optimization trace, the best parameters, the best fitness value and some additional information.

```splus
set.seed(123)
res = doTheEvolution(obj.fun, control = control)
print(res)
print(as.data.frame(res$opt.path))
print(autoplot(res, complete.trace = TRUE, log.fitness = TRUE, show.))
```

Take a glance at the examples in the inst/examples directory.

## Contact

Please address questions and missing features about the **ecr package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/ecr/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.



