library(ecr)
library(BBmisc)
library(devtools)

load_all()
load_all("../rpn/")

# polynomial we aim to approximate
target.fun = function(x) {
  #x^4 - x^3 + 2 * x^2 + 5
  x^4 + x^3 - 2 * x^2 + x + 10
}

lower = -5
upper = 5

obj.fun = function(obj, lower, upper) {
  force(obj)

  ob = length(which(obj == "("))
  cb = length(which(obj == ")"))
  if (ob != cb) {
    print(obj)
    stopf("Unequal!!! %i != %i", ob, cb)
  }

  approx.fun = function(x) {
    infix = rpn(obj, eval = FALSE)$infix
    eval(parse(text = infix))
  }

  fn = function(x) {
    abs(target.fun(x) - approx.fun(x))
  }
  return(integrate(fn, lower = lower, upper = upper)$value)
}

makeRandomExpression = function(depth = 1L) {
  nonterm = c("+", "-", "*")
  if (runif(1) < 0.3 || depth == 4) {
    ex = c(sample(c("x", as.character(round(runif(1L, 0, 10)))), 1L))
  } else {
    op = sample(nonterm, 1L)
    ex = c("(", makeRandomExpression(depth + 1L), makeRandomExpression(depth + 1L), op, ")")
  }
  return(ex)
}

generator = makeGenerator(
  generator = function(size, task, control) {
    makePopulation(lapply(1:size, function(x) makeRandomExpression()))
  },
  name = "Blub",
  supported = "custom",
  description = "..."
)

mutator = makeMutator(
  mutator = function(ind, task, control) {
    if (runif(1L) < 1) {
      # sample until we find a (non-)terminal element and skip all comma symbols
      poss = which(!(ind %in% c("(", ")")))
      n = length(ind)
      pos = sample(poss, 1L)
      el = ind[pos]
      #catf("Replacing element at pos %i: %s", pos, el)
      if (el %nin% c("+", "-", "*", "%")) {
        left = if (pos == 1) c() else ind[1:(pos - 1)]
        right = if (pos == n) c() else ind[(pos + 1):n]
      } else if (el %in% c("+", "-", "*", "%")) {
        # replace binary operator (op X Y)
        # I.e. search for second bracket
        pos.brr = pos + 1L
        pos2 = pos.brr
        open.brackets = 1L
        while (open.brackets > 0L) {
          #catf("Pos %i of %i (ob: %i)", pos2, n, open.brackets)
          pos2 = pos2 - 1L
          if (ind[pos2] == "(") {
            open.brackets = open.brackets - 1L
          } else if (ind[pos2] == ")") {
            open.brackets = open.brackets + 1L
          }
        }
        pos.brl = pos2
        right = if (pos.brr == n) c() else ind[(pos.brr + 1):n]
        left = if (pos.brl == 1) c() else ind[1:(pos.brl - 1)]
      }
      ind = c(left, makeRandomExpression(), right)
    }
    return(ind)
  },
  supported = "custom",
  name = "Expression Mutation",
  description = "..."
)

control = setupECRControl(
  n.population = 25L,
  n.offspring = 25L,
  n.elite = 1L,
  representation = "custom",
  survival.strategy = "comma",
  logger = setupOptPathLoggingMonitor(),
  stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 200L))
)

control = setupEvolutionaryOperators(
  control,
  recombinator = setupNullRecombinator(),
  mutator = mutator,
  generator = generator,
  parent.selector = setupTournamentSelector(k = 2L),
  survival.selector = setupGreedySelector()
)

task = makeOptimizationTask(fun = obj.fun, n.objectives = 1L, minimize = TRUE, objective.names = "abs dist")

set.seed(123)
res = doTheEvolution(task, control, more.args = list(lower = lower, upper = upper))

lapply(res$last.population$individuals, function(x) rpn(x)$infix)
print(rpn(res$best.param))

curve(target.fun(x), lower, upper, ylim = c(-100, 1000))

approx.fun = function(x) {
  sapply(x, function(y) rpn(res$best.param, vars = list(x = y))$value)
}

curve(approx.fun(x), lower, upper, add = TRUE, col = "blue")

autoplot(res) + ylim(c(0, 100))
