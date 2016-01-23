context("event dispatching")

test_that("event dispatcher works well", {
  eventDispatcher = setupEventDispatcher()

  x = NA

  # now register some events
  expect_error(eventDispatcher$registerAction("onNotExistentEvent", function(opt.state, ...) "foo"))
  eventDispatcher$register("onInitializedOptimization", function(opt.state, ...) {
    catf("init")
  })
  eventDispatcher$register("onInitializedOptimization", function(opt.state, ...) {
    x <<- 10L # modify test var
  })

  action.list = eventDispatcher$getActionList()
  expect_equal(length(action.list[["onInitializedOptimization"]]), 2L) # added two actions
  expect_true(all(sapply(action.list[["onInitializedOptimization"]], is.function)))
  expect_output(eventDispatcher$fireEvent("onInitializedOptimization", opt.state = NULL), regexp = "init")
  expect_equal(x, 10L)
})
