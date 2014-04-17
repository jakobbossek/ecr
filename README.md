# ecr
=====

This R package provides a powerful framework for Evolutionary Computing in R. The user can solve his objective functions using predefined evolutionary operators, i. e., operators for mutation, recombination and so on for several common representations, with only a few lines of code. Extending the framework with own operators is also possible. Additionaly there are various control options such as the survival strategy elitism, which allow maximal flexibility.


## Example
==========

The (soobench)[http://cran.r-project.org/web/packages/soobench/index.html] R package by Olaf Mersmann provides a collection of different single objective test functions commonly used for algorithm benchmarking. As an example we are going to search for the global optimum of the Ackley function. So first at all we import the neccessary packages and call he generater for the one-dimensional Ackley function.

```R
library(soobench)
library(ecr)

fn = generate_ackley_fun(1)
```

As a next step we generate an ecr *control object*, which holds all the neccessary parameters for the evolutionary algorithm. We decide ourself for the natural representation with real-valued numbers as the genotype, a population size of 10 individuals with 30 individuals being created by recombination and mutation in each generation. Further we decide to use a 'comma' survival strategy, i. e., all individuals from the i-th population will be replaced by the generated offspring. Since we want to keep the currently best individual, we set the elitism option to 1. We keep the remaining default parameters.

```R
control = ecr.control(
  population.size = 10L,
  offspring.size = 30L,
  representation = "float",
  survival.strategy = "comma",
  elite.size = 1L,
  n.params = 1L,
  n.targets = 1L
)
```

Now lets start the optimization process and print the result object.

```R
res = ecr(fn, control)
```

Contact
=======
Please address questions and missing features about the **ecr package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jbossek/ecr/issues) for this.



