#' @title
#' ecr: Evolutionary Computing in R
#'
#' @description
#' The \pkg{ecr} package offers a comprehensive collection of building blocks for
#' both single- and multi-objective evolutionary algorithms.
#'
#' A Few Words on Optimization
#' The task in global optimization is, given a set of objectives
#' \eqn{\mathcal{F} = \{f_1, \ldots, f_m\}} with \eqn{f_i : S \subseteq \mathbf{R}
#' \to \mathbf{R}}, \eqn{S} the region of feasible solutions, to find a solution
#' vector \eqn{\mathbf{x}^* \in S} with
#' \deqn{\mathbf{x}^* = arg\,min_{\mathbf{x}} (f_1(\mathbf{x}), \ldots, f_m(\mathbf{x}))}.
#' The defintion of \eqn{arg\,min} depends on the number of objectives in this
#' context. For a single objective, i.e., \eqn{m = 1} we are simply interested
#' in minimizing the function value, but in the case where \eqn{m \geq 2}, we need
#' another term of optimality, termed the Pareto-optimality. We do not want to
#' dive in to deeply in multi-objecitve optimization formalities here and refer the interested reader to
#' the literature.
#'
#' Problems in Optimization
#' Practisioners we are frequently faced with optimization problems. Unfortunately
#' these problems often consist of a variety of undesirable characteristics, e.g.,
#' nondifferentiability or noisyness. Besides, in particular in engineering, objective
#' functions are often not given as an analytical formula. Instead a function evaluation
#' is a run of a numerical simulation or even a physical experiment. Anyhow,
#' optimization problems at hand are seldom easily tractable and thus well-known
#' methods from mathematical optimization, e.g., Newton-Method or gradient descent
#' algorithms, often cannot be appied for solving.
#'
#' Evolutionary Optimization
#' Evolutionary Algorithms (EAs)
#' have proven very successful in a wide variety of real-world problems and they
#' can handle objective functions with one or more of the aforementioned properties
#' very well. In principle EAs follow a very easy principle which mimicks natural evoluation
#' due to Darwin: instead of iteratively working
#' on a single solution an EA generates a multiset of solution canditates, termned
#' individuals, in the decision space. This multiset forms the initial population.
#' Consequitively the following steps are
#' applied until a stopping condition is met: Select a good subset of the individuals
#' based on some selection criterion to form a socalled mating pool (selection
#' criteria work randomly and mostly assign higher selection probabilities for
#' individuals with high fitness, i.e., low fitness values). Then combine
#' individuals from the mating pool using recombination operators to form new
#' individuals which are further perturbed by mutation operators. Perform another
#' selection to choose the next population.
#'
#' Evolutionary Algorithms with \pkg{ecr}
#' All evolutionary algorithms are build up of the same evolutionary operators:
#' After initilizing an initial population, evolutionary operators, i.e., in particular
#' parental selection, recombination, mutation and survival selection are applied
#' until a stopping condition is met. This package offers a lot of ready to
#' use building blocks, which can easily be combined to set up evolutionary
#' algorithms, visualize results, monitor the optimization progress and compare
#' different evolutionary operators and/or parametrizations.
#'
#' @seealso \code{\link{setupECRControl}}, \code{\link{setupEvolutionaryOperators}},
#' \code{\link{doTheEvolution}}
#'
#' @docType package
#' @name ecrpackage
NULL
