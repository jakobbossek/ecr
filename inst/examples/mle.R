library(ecr)

# reproducibility
set.seed(1)

lambda = 5
x = rexp(100L, rate = lambda)

obj.fun = makeSingleObjectiveFunction(
  fn = function(lambda) {
    val = sum(dexp(x, rate = lambda, log = TRUE))
    return(val)
  },
  minimize = FALSE,
  par.set = makeParamSet(
    makeNumericParam("lambda", lower = 0.0001, upper = 10)
  )
)

control = setupECRControl(
  n.population = 10L,
  n.offspring = 10L,
  representation = "float",
  survival.strategy = "comma",
  n.elite = 4L,
  stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 100L))
)

res = doTheEvolution(obj.fun, control)

pdf(file = "ml_estimator.pdf", width = 8, height = 5)
hist(x, freq = FALSE)
curve(dexp(x, lambda), add = TRUE, lty = 3)
curve(dexp(x, res$best.param), add = TRUE, lty = 2, col = "red")
legend(x = 0.8, y = 3,
  legend = c(expression(lambda), expression(lambda[ML])),
  lty = c(3,2), col = c("black", "red")
)
dev.off()
