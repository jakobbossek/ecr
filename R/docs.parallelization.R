#' @title Parallelization in ecr
#'
#' @description
#' In ecr it is possible to parallelize different levels of computation
#' to make use, e.g., of multiple CP cores or nodes in a HPC cluster.
#' For maximal flexibility this is realized by means of the \pkg{parallelMap} package
#' (see the \href{https://github.com/berndbischl/parallelMap}{official
#' GitHub page} for instructions on how to set up parallelization).
#' The different levels of parallelization can be specified in the
#' \code{parallelStart*} function. At them moment the following levels are
#' available:
#' \describe{
#'   \item{\code{ecr.evaluateFitness}}{Do the fitness evaluation in parallel.}
#'   \item{\code{ecr.generateOffspring}}{The generation of offspring individuals is
#' performed in parallel.}
#' }
#'
#' Keep in mind that parallelization comes along with some overhead. Thus activating
#' parallelization, e.g., for evaluation a fitness function which is evaluated
#' lightning-fast, may result in higher computation time. However, if the function
#' evaluations are computationally more expensive, parallelization leads to
#' impressive running time benefits.
#'
#' @name ecr_parallelization
NULL
