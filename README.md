# ecr: Evolutionary Computing in R

Travis CI build status: [![Build Status](https://travis-ci.org/jakobbossek/ecr.svg?branch=master)](https://travis-ci.org/jakobbossek/ecr)

The **ecr** package provides a powerful framework for **Evolutionary Computing in R**. The user can solve his objective functions using predefined evolutionary operators, i. e., operators for mutation, recombination and so on for several common representations, with only a few lines of code. Extending the framework with own operators is also possible. Additionaly there are various control options such as the survival strategy elitism, which allow maximal flexibility.

# Installation instructions

The package will be available in a first version at [CRAN](http://cran.r-project.org) soon. If you are interested in trying out and playing around with the current github developer version use the [devtools](https://github.com/hadley/devtools) package and use the following command in R.

```splus
devtools::install_github("jakobossek/ecr")
```

## Example

In this section we want to optimize a one dimensional function with an Evolutionary Algorithm using just the evolutionary operators shipped with the package. A more in-depth introduction will be made available soon.

The [soobench](http://cran.r-project.org/web/packages/soobench/index.html) R package provides a collection of different single objective test functions commonly used in algorithm benchmarking. As an example we are going to search for the global optimum of the one-dimensional Rastrigin function. The function definition is located in the soobench package, but *ecr* needs the objective function to be of [otf](https://github.com/jakobbossek/otf) type. Fortunately there is a function which generates an otf function out of a soobench function.

```splus
library(soobench)
library(otf)
library(ecr)

obj.fun = makeSingleObjectiveFunctionFromSOOFunction("rastrigin", dimensions = 1L)

```

As a next step we generate an ecr *control object*, which holds all the neccessary parameters for the evolutionary algorithm. We decide ourself for the natural representation with real-valued numbers as the genotype, a population size of 20 individuals with 5 individuals being created by recombination and mutation in each generation. Furthermore we decide to use a 'plus' survival strategy, i. e., the current population and the offspring will be merged before survival selection takes place. Gauss mutation with a standard deviance of 0.005 serves as the mutation operator and we keep the intermediate recombination operator (which is the default for representation float). Moreover we define a maximal number of 50 generations.

```splus
control = ecr.control(
  population.size = 20L,
  offspring.size =5L,
  representation = "float",
  survival.strategy = "plus",
  n.params = 1L,
  n.targets = 1L,
  mutator = list(gaussMutator),
  mutator.control = list(mutator.gauss.sd = 0.005),
  max.iter = 50L)
```

Now lets start the optimization process and print the result object, which contains the optimization trace, the best parameters, the best fitness value and some additional information.

```splus
set.seed(123)
result = ecr(obj.fun, control = control)
print(result)
```

Take a glance at the examples in the inst/examples directory.

## Contact

Please address questions and missing features about the **ecr package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/ecr/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.



