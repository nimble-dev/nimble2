library(testthat)
library(nimble2)
nimbleOptions(enableDerivs = FALSE)

BROWSE_COMPILE_NIMBLE <- FALSE

test_that("nimbleFunction / RCfunction works and compiles through nCompile", {
  foo <- nimbleFunction(
    run = function(x = double(1)) {
      y <- x + 1
      return(y)
      returnType(double(1))
    },
    check = FALSE
  )
  foo(1:3)
  cfoo <- nCompiler::nCompile(foo)
  expect_equal(cfoo(1:3), 2:4)
  cfoo <- compileNimble(foo)
  expect_equal(cfoo(1:3), 2:4)
})
