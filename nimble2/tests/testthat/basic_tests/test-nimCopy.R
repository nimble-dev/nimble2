library(testthat)
library(nimble2)

nimbleOptions(enableDerivs = FALSE)
nCompiler::nOptions(pause_after_writing_files = FALSE)

test_that("basic nimCopy works", {
  message("Need to implement uncompiled nimCopy")
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
  exoect_equal(nCompiler::value(comp$nf1$model, "mu"), 100.1)
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
