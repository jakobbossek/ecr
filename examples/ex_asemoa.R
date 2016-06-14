library(smoof)

obj.fn = makeDTLZ2Function(dimensions = 2L, n.objectives = 2L)

# define the aspiration set, i.e., a set of reference points
aspiration.set = matrix(
  c(0.1531, 0.1594, 0.1656, 0.1719, 0.1781, 0.1844, 0.1906, 0.1969, 0.2031, 0.2094,
    0.2094, 0.2031, 0.1969, 0.1906, 0.1844, 0.1781, 0.1719, 0.1656, 0.1594, 0.1531),
  byrow = TRUE,
  nrow = 2L
)

\dontrun{
res = asemoa(
  task = obj.fn,
  n.population = 10L,
  aspiration.set = aspiration.set,
  max.evals = 1000L
)
}
