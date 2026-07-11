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

test_that("compileNimble works nimbleFunction using model$simulate", {
  message("model$simulate needs support for includeData argument")
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
      model$simulate(nodes, includeData = FALSE) # includeData is not yet supported when this is written
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
  
  set.seed(2)
  c_ll1 <- cnf$nf1$run()
  c_ll2 <- cnf$m$calculate(nodes)
  
  # work-around until includeData argument is supported
  nf_uncomp <- nimbleFunction(
    setup = function(model, nodes) {},
    run = function() {
      model$simulate(nodes) # includeData is not yet supported when this is written
      ans <- model$calculate(nodes)
      return(ans)
      returnType(double())
    },
    check = FALSE
  )
  
  
  set.seed(2)
  nfu1 <- nf_uncomp(m, nodes)
  r_ll1 <- nfu1$run()
  r_ll2 <- m$calculate(nodes)
  expect_equal(c_ll1, c_ll2)
  expect_equal(c_ll1, r_ll1)
  expect_equal(r_ll1, r_ll2)
  expect_equal(cnf$m$y, m$y)
  
  rm(nf1); gc()
})

test_that("compileNimble works nimbleFunction using model$calculateDiff and model$getLogProb", {
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
    setup = function(model, nodes) {
      first_logProb <- 0
      second_logProb <- 0
    },
    run = function(mu = 'numericVector', y = 'numericVector') {
      first_logProb <<- model$calculate(nodes)
      model$mu <<- mu
      model$y <<- y
      lpdiff <- model$calculateDiff(nodes)
      second_logProb <<- model$getLogProb(nodes)
      return(lpdiff)
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
  
  orig_mu <- m$mu
  orig_y <- m$y
  
  set.seed(10)
  new_mu <- rnorm(1)
  new_y <- rnorm(5)
  c_lpdiff <- cnf$nf1$run(new_mu, new_y)
  c_lp1 <- cnf$nf1$first_logProb
  c_lp2 <- cnf$nf1$second_logProb
  expect_equal(c_lpdiff, c_lp2 - c_lp1)
  
  cnf$m$mu <- orig_mu
  cnf$m$y <- orig_y
  r_lp1 <- cnf$m$calculate(nodes)
  cnf$m$mu <- new_mu
  cnf$m$y <- new_y
  r_lpdiff <- cnf$m$calculateDiff(nodes)
  r_lp2 <- cnf$m$getLogProb(nodes)
  expect_equal(r_lpdiff, r_lp2 - r_lp1)
  expect_equal(r_lpdiff, c_lpdiff)
  
  r_lpdiff <- nf1$run(new_mu, new_y)
  r_lp1 <- nf1$first_logProb
  r_lp2 <- nf1$second_logProb
  expect_equal(r_lpdiff, r_lp2 - r_lp1)
  expect_equal(r_lpdiff, c_lpdiff)
  
  rm(nf1); gc()
})


