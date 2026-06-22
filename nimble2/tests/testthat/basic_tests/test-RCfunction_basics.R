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

test_that("compileNimble works with model and nimbleFunction", {
  code <- quote({
    tau ~ dunif(0, 100)
    mu ~ dnorm(0, 1)
    for (i in 1:5) {
      y[i] ~ dnorm(mu, var = tau)
    }
  })

  inits <- list(tau = 25, mu = 0)
  data <- list(y = rnorm(5))

  mclass <- nimbleModel::nimbleModel(code, inits = inits, data = data, returnClass = TRUE)
  m <- mclass$new()

  nf <- nimbleFunction(
    setup = function(model) {},
    run = function(v = double(1)) {
      model$y <<- v
    },
    check = FALSE
  )

  nf1 <- nf(m)
  cnf <- compileNimble(nf1, m)
  cnf$m$calculate()
  expect_equal(cnf$m$y, m$y)
  cnf$nf1$run(1:5)
  expect_equal(cnf$m$y, 1:5)
})
