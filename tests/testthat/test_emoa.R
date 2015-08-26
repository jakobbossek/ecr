context("Evolutionary Multi-Objective Algorithms")

test_that("preimplemented EMOAs work well", {

  #FIXME: is something like this in BBmisc? Needed that multiple times now.
  printList = function(l) {
    ns = names(l)
    pairs = sapply(ns, function(n) {
      paste0(n, l[[n]], sep = "=")
    })
    collapse(pairs, ", ")
  }

  expect_is_pareto_approximation = function(pf, n.obj, algo, prob, algo.pars) {
    info.suffix = sprintf("Algo '%s' failed on problem '%s' with params '%s'",
      algo, prob, printList(algo.pars))
    expect_equal(nrow(pf), length(which.nondominated(t(pf))), info = paste0(info.suffix, "Not all returned points are nondominated."))
    expect_true(all(is.numeric(pf)), info = paste0(info.suffix, "Not all returned points are numeric."))
    expect_equal(ncol(pf), n.obj, info = paste0(info.suffix, "Number of columns is not equal to the number of objectives."))
  }

  fns = list(
    zdt1 = smoof::makeZDT1Function(dimensions = 2L),
    zdt2 = smoof::makeZDT2Function(dimensions = 2L),
    zdt3 = smoof::makeZDT3Function(dimensions = 2L)
  )
  max.evals = 100L

  # test NSGA-II and SMS-EMOA
  for (emoa in c("nsga2", "smsemoa")) {
    for (n.pop in c(5, 10, 15)) {
      for (fn in names(fns)) {
        res = do.call(emoa, list(
          task = makeOptimizationTask(fns[[fn]]),
          n.population = n.pop,
          n.offspring = 5L,
          max.evals = max.evals)
        )
        expect_is_pareto_approximation(res$pareto.front, 2L, emoa, fn,
          list(n.pop = n.pop, n.offspring = 5L, max.evals = max.evals)
        )
      }
    }
  }

  # test Aspiration-Set EMOA (AS-EMOA)
  for (n.pop in c(5, 10)) {
    task = makeOptimizationTask(makeZDT3Function(dimensions = 2L))
    aspiration.set = matrix(
      c(0.2, 0.25,
        0.21, 0.2,
        0.18, 0.4), ncol = 3L, byrow = FALSE
    )
    res = asemoa(task, n.population = n.pop, max.evals = max.evals,
      aspiration.set = aspiration.set, n.archive = 5L)
    expect_is_pareto_approximation(res$pareto.front, 2L, "asemoa", "ZDT3",
      list(n.pop = n.pop, n.archive = 5L))
  }
})
