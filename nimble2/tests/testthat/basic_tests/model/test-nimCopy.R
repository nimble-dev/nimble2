library(testthat)
library(nimble2)

nimbleOptions(enableDerivs = FALSE)
nCompiler::nOptions(pause_after_writing_files = FALSE)

test_that("basic nimCopy works", {
  ## To-do:
  ## Add tests with different to/from combinations.
  ## Also test operation after resetting nLists in a modelValues
  set.seed(1)
  code <- quote({
    tau ~ dunif(0, 100)
    mu ~ dnorm(0, 1)
    for (i in 1:5) {
      y[i] ~ dnorm(mu, var = tau)
    }
    for(i in 1:5) {
      for(j in 1:5) {
        z[i, j] ~ dnorm(mu, var = tau)
      }
    }
  })

  inits <- list(tau = 25, mu = 0,
                z = matrix(rnorm(25), nrow = 5))
  data <- list(y = rnorm(5))

  mclass <- nimbleModel::nimbleModel(code, inits = inits, data = data, returnClass = TRUE)
  m <- mclass$new()

  ## test <- nimbleModel:::modelValues(m)
  ## obj <- test$new()

  nf <- nimbleFunction(
    setup = function(model, nodes) {
      mvClass <- nimbleModel:::modelValues(model)
      mv <- mvClass$new()
      mv$resize(2)
    },
    methods = list(
      go = function() {
        nimCopy(from = model, nodes = nodes, to = mv, row = 1)
      }
    ),
    check = FALSE
  )

  nodes <- list(nimbleModel::varRangeClass$new("mu"),
                nimbleModel::varRangeClass$new("z[2:4, 1:3]"))

  nf1 <- nf(m, nodes)

  comp <- compileNimble(nf1, m)

  comp$nf1$mv["mu", 1] <- 1.2
  expect_equal(comp$nf1$mv["mu", 1], 1.2)
  nCompiler::value(comp$nf1$model, "z") <- matrix(1:25, nrow = 5)
  expect_equal(nCompiler::value(comp$nf1$model, "z"), matrix(1:25, nrow = 5))
  nCompiler::value(comp$nf1$model, "mu") <- 100.1
  expect_equal(nCompiler::value(comp$nf1$model, "mu"), 100.1)
  comp$nf1$go()
  expect_equal(comp$nf1$mv["mu", 1], 100.1)
  z_expected <- matrix(0, nrow = 5, ncol = 5)
  z_expected[2:4, 1:3] <- matrix(1:25, nrow = 5)[2:4, 1:3]
  expect_equal(comp$nf1$mv["z", 1], z_expected)
  expect_equal(nCompiler:::value(comp$nf1$mv, "z") |> as.list(),
               list(z_expected, matrix(0, nrow = 5, ncol = 5)))
  comp$nf1 <- comp$m <- NULL
  rm(comp); gc()
})

test_that("nimCopy with nodesTo, logProb, logProbOnly and missing nodes matches uncompiled", {
  code <- quote({
    mu ~ dnorm(0, sd = 2)
    for (i in 1:3) {
      x[i] ~ dnorm(mu, 1)
      for (j in 1:4) y[i, j] ~ dnorm(x[i], 1)
    }
    w <- mu + 1
  })
  make_model <- function() {
    m <- nimbleModel::nimbleModel(code, data = list(y = matrix(1:12, 3)),
                                  inits = list(mu = 0.2, x = 1:3), returnClass = TRUE)$new()
    m$calculate()
    m
  }
  nf <- nimbleFunction(
    setup = function(model, nodes) {
      mv <- nimbleModel:::modelValues(model)$new()
      mv$resize(3)
    },
    methods = list(
      toMV = function() {
        nimCopy(from = model, to = mv, nodes = nodes, row = 1, logProb = TRUE)
        nimCopy(from = model, to = mv, row = 2, logProbOnly = TRUE)
        nimCopy(from = model, to = mv, row = 3)
      },
      back = function() {
        nimCopy(from = mv, to = model, nodes = "x[2:3]", nodesTo = "x[1:2]", row = 1)
        nimCopy(from = mv, to = model, nodes = "mu", row = 1, logProb = TRUE)
        nimCopy(from = mv, to = mv, row = 2, rowTo = 3, logProbOnly = TRUE)
      }
    ),
    check = FALSE
  )
  run <- function(obj, model) {
    obj$toMV()
    ans <- list(
      mu1 = obj$mv["mu", 1], x1 = obj$mv["x", 1], lpx1 = obj$mv["logProb_x", 1], lpy1 = obj$mv["logProb_y", 1],
      x2 = obj$mv["x", 2], lpy2 = obj$mv["logProb_y", 2], y3 = obj$mv["y", 3], lpy3 = obj$mv["logProb_y", 3]
    )
    model$mu <- 5
    model$logProb_mu <- 0
    model$x <- c(10, 20, 30)
    obj$mv["logProb_y", 3] <- matrix(0, 3, 4)
    obj$back()
    c(ans, list(x_after = model$x, mu_after = model$mu, lpmu_after = model$logProb_mu,
                lpy3_after = obj$mv["logProb_y", 3]))
  }
  nodes <- c("mu", "x[2:3]")
  m <- make_model()
  nf1 <- nf(m, nodes)
  r_ans <- run(nf1, m)
  m0 <- make_model()
  expect_equal(r_ans$mu1, 0.2)
  expect_equal(r_ans$x1, c(0, 2, 3))
  expect_equal(r_ans$lpx1, c(0, m0$logProb_x[2:3]))
  expect_equal(r_ans$lpy1, matrix(0, 3, 4)) # y is not in nodes
  expect_equal(r_ans$x2, c(0, 0, 0)) # logProbOnly
  expect_equal(r_ans$lpy2, m0$logProb_y)
  expect_equal(r_ans$y3, m0$y) # nodes missing: all variables
  expect_equal(r_ans$lpy3, matrix(0, 3, 4)) # but not logProb variables
  expect_equal(r_ans$x_after, c(2, 3, 30)) # nodesTo
  expect_equal(r_ans$mu_after, 0.2)
  expect_equal(r_ans$lpmu_after, m0$logProb_mu) # logProb from a modelValues, using the model
  expect_equal(r_ans$lpy3_after, m0$logProb_y)

  m2 <- make_model()
  nf2 <- nf(m2, nodes)
  comp <- compileNimble(nf2, m2)
  c_ans <- run(comp$nf2, comp$m2)
  expect_equal(c_ans, r_ans)
  comp$nf2 <- comp$m2 <- NULL
  rm(comp); gc()
})
