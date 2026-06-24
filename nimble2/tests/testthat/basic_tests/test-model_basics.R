library(testthat)
library(nimble2)
nimbleOptions(enableDerivs = FALSE)

BROWSE_COMPILE_NIMBLE <- FALSE

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

test_that("compileNimble works nimbleFunction using model$calculate", {
  nimbleOptions(enableDerivs = FALSE)
  .GlobalEnv$BROWSE_COMPILE_NIMBLE <- FALSE
  
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
  nodes <- m$getDependencies("mu")
  nf <- nimbleFunction(
    setup = function(model, nodes) {},
    run = function() {
      ans <- model$calculate(nodes)
      return(ans)
      returnType(double())
    },
    check = FALSE
  )
  
  nf1 <- nf(m, nodes)
  cnf <- compileNimble(nf1, m)
  c_ll <- cnf$m$calculate()
  r_ll <- m$calculate()
  expect_equal(c_ll, r_ll)
  
  expect_equal(cnf$m$y, m$y)

  c_ll1 <- cnf$nf1$run()
  c_ll2 <- cnf$m$calculate(nodes)
  r_ll <- m$calculate(nodes)
  expect_equal(c_ll1, c_ll2)
  expect_equal(c_ll1, r_ll)
  
  cnf$m$mu <- 0.63
  m$mu <- 0.63
  c_ll1 <- cnf$nf1$run()
  c_ll2 <- cnf$m$calculate(nodes)
  r_ll <- m$calculate(nodes)
  
  expect_equal(c_ll1, c_ll2)
  expect_equal(c_ll1, r_ll)
  
  rm(nf1); gc()
})

