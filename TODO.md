TODO
====

  - think about general structure of function(s), at all costs follow the "DRY" principle, i.e., "don't repeat yourself"
  - think about what type of elements should be wrapped in S3 classes:
      - population (has size, individuals, fitness values of individuals),
      - individual (has fitness value, date of birth, param values, age (for age-based selection methods))
      - optimization trace (data frame with best individuals and their fitness values for each iteration, date of birth and some more measures)
  - think about the best programmatical interface solution for the user. It should be as simple as
    possible to extend the functionality and write propriatary mutation, selection ... funs.
  - check Bernds *ParamHelpers* package thorougly. Maybe makeOptPath and all the make...Param funs can
    be used in this package without adaptions
  - what types of traget funs should be supported? Just funs with numeric-only parameters like the
    soobnech funs or allow discrete params as well? Would be very cool if arbitrary functions could optimized (for example the)
  - research for common (and not so common) mutation, selection and recombination operators
  - investigate existing R packages for evolutionary optimization (RFreak, GA, ...)

SOME MORE THOUGTHS
==================

  - allow chaining of mutators, i. e., allow to apply more than one mutator to the offspring
  - allow chaining of pre-/postprocess functions, for example correctBounds if box constraints must be met
  - allow arbitrary genotype-phenotype-mappings?
  - give the possibility to mutate the mutation operators as well, i. e., see them as a part of the individuum
  - follow Bernds suggestion to allow **evolutionary multi-criteria optimization** as well?
  - At least common operators for initial population generation, selection and mutation as well as recombination of real-valued, binary representations and permutations should be provided
  - Check for bottlenecks and implement time consuming stuff in C/C++
  - Use Bernds *parallelMap* to evaluate the fitness function on population/offspring
  - allow **parameter control**, i. e., the change of parameters such as the mutation step size *during* the optimization process. For example implement Rechenbergs 1/5 rule or decreasing sigma with ongoing generations?
  - How to deal with constraints other than box-constraints? Force the use to integrate them into the objective function in the form ```if(x1 + x2 > 2) return(Inf)```?
