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

test_that("compileNimble works with nimbleFunction using model$calculate", {
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

test_that("compileNimble works with nimbleFunction using model$simulate", {
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
      model$simulate(nodes, includeData = FALSE)
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

  set.seed(2)
  nfu1 <- nf1# nf_uncomp(m, nodes)
  r_ll1 <- nfu1$run()
  r_ll2 <- m$calculate(nodes)
  expect_equal(c_ll1, c_ll2)
  expect_equal(c_ll1, r_ll1)
  expect_equal(r_ll1, r_ll2)
  expect_equal(cnf$m$y, m$y)

  rm(nf1); gc()
})

test_that("compileNimble works with nimbleFunction using model$calculateDiff and model$getLogProb", {
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

test_that("basic getParam and getBound works", {

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
  m$calculate()

  nf <- nimbleFunction(
    setup = function(model, node1, param, node2) {
    },
    methods = list(
      checkParam = function() {
        ans <- model$getParam(node1, param)
        return(ans)
        returnType(double())
      },
      checkBounds = function() {
        ans1 <- model$getBound(node1, "lower")
        ans2 <- model$getBound(node1, "upper")
        ans3 <- model$getBound(node2, "lower")
        ans4 <- model$getBound(node2, "upper")
        ans <- c(ans1, ans2, ans3, ans4)
        return(ans)
        returnType(double(1))
      }
    ),
    check = FALSE
  )

  nf1 <- nf(m, "y[3]", "sd", "tau")
  expect_equal(nf1$checkParam(), 5)
  expect_equal(nf1$checkBounds(), c(-Inf, Inf, 0, 100))

  comp <- compileNimble(m, nf1)
  expect_equal(comp$nf1$checkParam(), 5)
  expect_equal(comp$nf1$checkBounds(), c(-Inf, Inf, 0, 100))
  comp$nf1 <- NULL
  rm(nf1, comp)
  gc()
})

test_that("getParam works for vector and matrix parameters, compiled", {
  code <- quote({
    for (i in 1:2) {
      x[i, 1:3] ~ dmnorm(mu[i, 1:3], prec = P[1:3, 1:3])
    }
    W[1:3, 1:3] ~ dwish(R = P[1:3, 1:3], df = 5)
    z[1:3] ~ ddirch(alpha[1:3])
    k ~ dcat(p[1:3])
  })
  P <- crossprod(matrix(c(2, .3, .1, .3, 1.5, .2, .1, .2, 1), 3))
  mu <- matrix(c(.1, .2, .3, 1, 2, 3), nrow = 2, byrow = TRUE)
  inits <- list(
    mu = mu, P = P, alpha = c(1, 2, 3), p = c(.2, .3, .5),
    x = matrix(1:6, 2), W = 2 * P, z = c(.2, .3, .5), k = 2
  )
  mclass <- nimbleModel::nimbleModel(code, inits = inits, returnClass = TRUE)
  m <- mclass$new()
  m$calculate()

  nf <- nimbleFunction(
    setup = function(model, xnodes) {},
    methods = list(
      getMean = function(i = integer()) {
        ans <- model$getParam(xnodes[i], "mean")
        return(ans)
        returnType(double(1))
      },
      getCov = function() {
        ans <- model$getParam("x[1, 1:3]", "cov")
        return(ans)
        returnType(double(2))
      },
      getWishS = function() {
        ans <- model$getParam("W", "S")
        return(ans)
        returnType(double(2))
      },
      getAlpha = function() {
        ans <- model$getParam("z", "alpha")
        return(ans)
        returnType(double(1))
      },
      getProb = function() {
        ans <- model$getParam("k", "prob")
        return(ans)
        returnType(double(1))
      }
    ),
    check = FALSE
  )
  nf1 <- nf(m, c("x[1, 1:3]", "x[2, 1:3]")) # uncompiled, xnodes[i] indexes these names
  check <- function(obj) {
    expect_equal(obj$getMean(1), mu[1, ])
    expect_equal(obj$getMean(2), mu[2, ])
    expect_equal(obj$getCov(), solve(P))
    expect_equal(obj$getWishS(), solve(P))
    expect_equal(obj$getAlpha(), c(1, 2, 3))
    expect_equal(obj$getProb(), c(.2, .3, .5))
  }
  check(nf1)

  comp <- compileNimble(m, nf1)
  for (v in names(inits)) comp$m[[v]] <- inits[[v]]
  comp$m$calculate()
  check(comp$nf1)
  comp$nf1 <- NULL
  rm(nf1, comp)
  gc()
})

test_that("model$calculate(), calculateDiff(), getLogProb() and simulate() with no nodes use the whole model, compiled", {
  nimbleOptions(enableDerivs = FALSE)
  code <- quote({
    mu ~ dnorm(0, sd = s) # s is RHS-only
    for (i in 1:4) {
      x[i] ~ dnorm(mu, 1)
      w[i, 1:2] <- c(x[i], 2 * mu)
    }
    for (j in 1:3) y[j] ~ dnorm(x[j], 1)
  })
  constants <- list(s = 2)
  data <- list(y = c(0.5, -1, 2))
  inits <- list(mu = 0.2, x = c(0.1, -0.3, 1, 2))
  mclass <- nimbleModel::nimbleModel(code, constants = constants, data = data, inits = inits, returnClass = TRUE)
  m <- mclass$new()
  nf <- nimbleFunction(
    setup = function(model) {},
    run = function(simData = logical()) {
      a <- model$calculate()
      b <- getLogProb(model)
      d <- model$calculateDiff()
      if (simData) {
        model$simulate(includeData = TRUE)
      } else {
        simulate(model)
      }
      e <- model$calculate()
      return(c(a, b, d, e))
      returnType(double(1))
    },
    check = FALSE
  )
  nf1 <- nf(m)
  expected <- dnorm(0.2, 0, 2, log = TRUE) + sum(dnorm(inits$x, 0.2, 1, log = TRUE)) +
    sum(dnorm(data$y, inits$x[1:3], 1, log = TRUE))
  # uncompiled
  set.seed(1)
  r_ans <- nf1$run(FALSE)
  expect_equal(r_ans[1:3], c(expected, expected, 0))
  expect_equal(m$y, data$y)
  expect_equal(m$w[, 1], m$x)
  expect_equal(r_ans[4], m$calculate(m$getNodeNames()))

  m$mu <- inits$mu
  m$x <- inits$x
  m$calculate()
  cnf <- compileNimble(nf1, m)
  set.seed(1)
  c_ans <- cnf$nf1$run(FALSE)
  expect_equal(c_ans[1:3], c(expected, expected, 0))
  expect_equal(cnf$m$y, data$y) # data are not simulated by default
  expect_false(isTRUE(all.equal(cnf$m$x, inits$x)))
  expect_equal(cnf$m$w[, 1], cnf$m$x)
  expect_equal(c_ans[4], cnf$m$calculate(cnf$m$getNodeNames()))
  c_ans <- cnf$nf1$run(TRUE)
  expect_false(isTRUE(all.equal(cnf$m$y, data$y)))
  expect_equal(c_ans[4], cnf$m$calculate(cnf$m$getNodeNames()))
  rm(nf1); gc()
})
