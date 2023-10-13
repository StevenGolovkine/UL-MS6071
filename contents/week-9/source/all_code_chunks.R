################################################################################
# Script file for Week 09 - Debugging and profiling
################################################################################



library(profvis)

profvis::profvis({
  f <- function(x) {
      y <- integer()
      for (i in 1:1e4) {
        y <- c(y, i + x)
      }
      g(x)
      h(x)
  }

  g <- function(x) {
      y <- integer()
      for (i in 1:1e4) {
        y <- c(y, i + x)
      }
      h(x)
  }

  h <- function(x) {
      y <- integer()
      for (i in 1:1e4) {
        y <- c(y, i + x)
      }
      y
  }

  f(10)
})

profvis::profvis({
  x <- integer()
  for (i in 1:1e4) {
    x <- c(x, i)
  }
})

library(bench)

x <- runif(100)
bench::mark(exp(4 * log(x)), x^4, iterations = 1000)



