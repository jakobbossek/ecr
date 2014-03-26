library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(esoo)
}

test_dir("tests/testthat")
