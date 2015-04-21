# Four cases:
How to solve that elegantly?
1. Standard genotype, single-objective
-> x and **best** y can be saved in optPath as well as entire population
2. Standard genotype, multi-objective
-> x and y can be saved in optPath, but there is no *best*
3. Custom genotype, single-objective
-> **only** (best) y can be saved (x values not scalar or vector)
4. Custom genotype, multi-objective
-> **only** y values can be saved (there is no best and x values not scalar)

Possible solutions (these are just some considerations)
- Principally do not use optPath to store x-values. Always save them in a list -> this way we need not distinguish between all the genotype variants
- setup 'ecr templates': one for custom representations and one for standard representations. This way we could always store all the stuff in the opt.path for default without hindering and we would still have a mess for custom representations :-(
- 

Moreover!
optPath is passed to terminator objects.
-> Terminators need to become ecr_operators with supported.objectives field (like selectors). E.g. stop if global optimum is approximated is not suitable as an emoa stopping condition.



:-(
