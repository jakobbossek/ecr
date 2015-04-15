context("extras in opt.path")

test_that("user defined extras are stored in opt.path", {
  fn = smoof::makeSphereFunction(2L)

  # define our extras fun which computes more or less useful stuff
  extras.fun = function(population) {
    list(
      "num1" = sd(population$fitness), # stuff
      "num2" = min(sapply(population$individuals, min)), # individuals
      "disc" = sample(letters[1:10], 1L) # discrete stuff
    )
  }

  ctrl = setupECRControl(
    n.population = 5L,
    n.offspring = 5L,
    survival.strategy = "plus",
    representation = "float",
    stopping.conditions = setupStoppingConditions(max.iter = 2L),
    extras.fun = extras.fun
  )
  ctrl = setupEvolutionaryOperators(ctrl)

  res = doTheEvolution(fn, ctrl)
  opt.path = as.data.frame(res$opt.path)
  print(opt.path)

  # check that corresponding column names exist
  expected.names = c("num1", "num2", "disc")
  expect_true(isSubset(expected.names, colnames(opt.path)))

  # check that they have the correct type
  expect_true(is.numeric(opt.path[["num1"]]))
  expect_true(is.numeric(opt.path[["num2"]]))
  expect_true(is.factor(opt.path[["disc"]]))
  expect_true(all(as.character(opt.path[["disc"]]) %in% letters[1:10]))
})
