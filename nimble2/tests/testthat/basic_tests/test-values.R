library(testthat)
library(nimble2)

nimbleOptions(enableDerivs = FALSE)

nCompiler::nOptions(pause_after_writing_files = FALSE)

#BROWSE_COMPILE_NIMBLE <- FALSE

message("values needs updating to work with indexing singletons and blanks.")

test_that("values(mode, nodes) works", {
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

  nf <- nimbleFunction(
    setup = function(model, nodes) {},
    methods = list(
      get = function() {
        v <- values(model, nodes)
        return(v)
        returnType(double(1))
      },
      get_and_use = function() {
        v <- 2 * values(model, nodes) + 1
        v <- values(model, nodes) + v
        return(v)
        returnType(double(1))
      },
      set = function(v = double(1)) {
        values(model, nodes) <<- v
      },
      set_with_use = function(v = double(1)) {
        values(model, nodes) <<- 2 * v
        values(model, nodes) <<- values(model, nodes) + v
      }
    ),
    check = FALSE
  )

  # To-Do: check all these cases
  # At the moment of this writing, only ranges are supported
  #  (not blanks nor singletons)
  #  (and blanks are not supported in varRangeClass, a higher level issue)
  #   nodes <- list(nimbleModel::varRangeClass$new("y[1:3]"),
  #                 nimbleModel::varRangeClass$new("y[2]"),
  #                 nimbleModel::varRangeClass$new("y"),
  # #                nimbleModel::varRangeClass$new("y[]"),
  #                 nimbleModel::varRangeClass$new("mu"),
  #                 nimbleModel::varRangeClass$new("z[2:4, 1:3]"),
  #                 nimbleModel::varRangeClass$new("z[2:4, 2]"),
  #                 nimbleModel::varRangeClass$new("z[3, 3]"),
  # #                nimbleModel::varRangeClass$new("z[2:4, ]"),
  # #                nimbleModel::varRangeClass$new("z[,]"),
  #                 nimbleModel::varRangeClass$new("z"))
  # multiCopier <- nimbleModel:::makeMultiCopier(m, nodes)

  nodes <- list(nimbleModel::varRangeClass$new("y[1:3]"),
                nimbleModel::varRangeClass$new("z[2:4, 1:3]"))

  nf1 <- nf(m, nodes)
  comp <- compileNimble(nf1, m)

  ### get
  #
  # uncompiled
  expectedU <- c(m$y[1:3], as.numeric(m$z[2:4, 1:3]))
  nf1$get()
  expect_equal( expectedU,
               nf1$get())
  # compiled
  expected <- c(comp$m$y[1:3], as.numeric(comp$m$z[2:4, 1:3]))
  expect_equal( expected,
               comp$nf1$get())
  expect_equal(expected, expectedU)

  ### get_and_use
  #
  # uncompiled
  expect_equal(nf1$get_and_use(), 2*expectedU + 1 + expectedU)
  # compiled
  expect_equal(comp$nf1$get_and_use(), 2*expected + 1 + expected)

  ### set
  #
  # uncompiled
  new_values <- rnorm(length(expected))
  nf1$set(new_values)
  expectedU <- c(m$y[1:3], as.numeric(m$z[2:4, 1:3]))
  expect_equal(new_values, expectedU)
  # compiled
  comp$nf1$set(new_values)
  expected <- c(comp$m$y[1:3], as.numeric(comp$m$z[2:4, 1:3]))
  expect_equal(new_values, expected)

  ### set_with_use
  #
  # uncompiled
  new_values <- rnorm(length(expected))
  nf1$set_with_use(new_values)
  expectedU <- c(m$y[1:3], as.numeric(m$z[2:4, 1:3]))
  expect_equal(3*new_values, expectedU)

  comp$nf1$set_with_use(new_values)
  expected <- c(comp$m$y[1:3], as.numeric(comp$m$z[2:4, 1:3]))
  expect_equal(3*new_values, expected)
})
