library(ecr)
library(plyr)
library(reshape2)

set.seed(1)

# knapsack with 10 objects A, B, ..., J with weight w_i and value v_i
ks = data.frame(
  object = LETTERS[1:10],
  weight = c(10, 11, 4, 2, 2, 9, 3, 5, 15, 4),
  value  = c(5, 10, 1, 6, 3, 5, 3, 19, 23, 1)
)

# knapsack capacity
ks.limit = 25

# objective function to be maximized, i.e., total value of bagged objects,
# under the restriction of the total weight being lower than the capacity.
obj.fun = makeSingleObjectiveFunction(
  fn = function(x) {
    val = sum(x * ks$value)
    weight = sum(x * ks$weight)
    if (weight > ks.limit)
      return(0)
    return(val)
  },
  minimize = FALSE, # here we maximize
  par.set = makeParamSet(
    makeIntegerVectorParam(id = "x", lower = 0, upper = 1, len = nrow(ks))
  )
)

# use "natural" binary representation x = (x_1, ..., x_n) with x_i = 1 means,
# that object i is bagged and x_i = 0 otherwise.
control = setupECRControl(
  representation = "binary",
  n.population = 25,
  n.offspring = 10,
  monitor = NULL,
  logger = setupOptPathLoggingMonitor(),
  survival.strategy = "plus",
  stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 100L))
)

# run algorithm
res = doTheEvolution(obj.fun, control)

# extract EA knapsack solution
print(res$best.value)
print(res$best.param)
print(ks[as.logical(res$best.param), ])

# plot optimization path
op = as.data.frame(res$opt.path)
op = ddply(op, .variables = c("dob"), summarize, mean = mean(y), max = max(y))
op = melt(op, "dob", value.name = "Value", variable.name = "Stat")

pl = ggplot(op, aes(x = dob, y = Value, colour = Stat)) + geom_line()
pl = pl + xlab("Generation") + ylab("Total value")
print(pl)
#ggsave(filename = "KP_trace.pdf", width = 6, height = 3)
