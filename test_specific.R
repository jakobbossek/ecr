library(methods)
library(devtools)
library(testthat)
library(soobench)

load_all(".")

f = generate_sphere_function(2)
plot(f)

res = esoo(f, 1000, 5)
print(res)
