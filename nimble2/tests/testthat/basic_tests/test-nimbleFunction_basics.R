library(testthat)
library(nimble2)
nimbleOptions(enableDerivs = FALSE)

BROWSE_COMPILE_NIMBLE <- FALSE

test_that("nimbleFunction with setup code compiles and works", {
  nf <- nimbleFunction(
    setup = function() {
      x <- 1:3
    },
    run = function(y = double(1)) {
      ans <- x + y
      return(ans)
      returnType(double(1))
    },
    check = FALSE
  )
  expect_false(is.nf(nf))
  expect_true(is.nfGenerator(nf))
  nf1 <- nf()
  expect_true(is.nf(nf1))
  expect_equal(nf1$run(1:3), 2 * (1:3))
  
  cnf <- compileNimble(nf1)
  expect_equal(cnf$run(2:4), 1:3 + 2:4)
  expect_equal(cnf$x, 1:3)
  cnf$x <- 11:13
  expect_equal(cnf$run(2:4), 11:13 + 2:4)
})