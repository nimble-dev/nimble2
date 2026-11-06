library(testthat)
library(nimble2)
nimbleOptions(enableDerivs = FALSE)

test_that("math functions work in nimbleFunctions, uncompiled and compiled", {
  nf_scalar <- nimbleFunction(
    run = function(x = double()) {
      y <- step(x) + nimStep(x) + equals(x, 2) + nimEquals(x, 2) + pow(x, 2) +
        logit(0.3) + expit(x) + probit(0.3) + cloglog(0.3) + cube(x) + loggam(2.5)
      return(y)
      returnType(double())
    }
  )
  nf_vector <- nimbleFunction(
    run = function(v = double(1)) {
      return(inprod(v, v))
      returnType(double())
    }
  )
  nf_matrix <- nimbleFunction(
    run = function(A = double(2)) {
      return(inverse(A) * logdet(A))
      returnType(double(2))
    }
  )
  expected <- function(x) {
    2 * (x >= 0) + 2 * (x == 2) + x^2 + qlogis(0.3) + plogis(x) + qnorm(0.3) +
      log(-log(1 - 0.3)) + x^3 + lgamma(2.5)
  }
  A <- matrix(c(2, 0.5, 0.5, 1), 2)
  cnf_scalar <- compileNimble(nf_scalar)
  cnf_vector <- compileNimble(nf_vector)
  cnf_matrix <- compileNimble(nf_matrix)
  for (x in c(-1, 0, 2)) {
    expect_equal(nf_scalar(x), expected(x))
    expect_equal(cnf_scalar(x), expected(x))
  }
  expect_equal(nf_vector(c(1, 2, 3)), 14)
  expect_equal(cnf_vector(c(1, 2, 3)), 14)
  expect_equal(nf_matrix(A), solve(A) * log(det(A)))
  expect_equal(cnf_matrix(A), solve(A) * log(det(A)))
})
