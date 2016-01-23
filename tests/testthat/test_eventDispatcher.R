context("event dispatching")

test_that("event dispatcher works well", {
  eventDispatcher = setupEventDispatcher()

  x = NA

  # now register some events
  expect_error(eventDispatcher$registerAction("onNotExistentEvent", function(opt.state, ...) "foo"))
  eventDispatcher$register("onEAInitialized", function(opt.state, ...) {
    catf("init")
  })
  eventDispatcher$register("onEAInitialized", function(opt.state, ...) {
    x <<- 10L # modify test var
  })

  action.list = eventDispatcher$getActionList()
  expect_equal(length(action.list[["onEAInitialized"]]), 2L) # added two actions
  expect_true(all(sapply(action.list[["onEAInitialized"]], is.function)))
  expect_output(eventDispatcher$fireEvent("onEAInitialized", opt.state = NULL), regexp = "init")
  expect_equal(x, 10L)
})
