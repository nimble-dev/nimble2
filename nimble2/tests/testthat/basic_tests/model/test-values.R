library(testthat)
library(nimble2)

nimbleOptions(enableDerivs = FALSE)

nCompiler::nOptions(pause_after_writing_files = FALSE)

#BROWSE_COMPILE_NIMBLE <- FALSE

message("values needs updating to work with indexing singletons and blanks.")

test_that("values(model, nodes) works", {
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
  #nf1$get()
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
  comp$nf1 <- comp$m <- NULL
  rm(comp); gc()
})

test_that("values(model, nodes[i], mode = ...) works", {
  set.seed(1)
  code <- quote({
    mu ~ dnorm(0, 1)
    for (i in 1:5) {
      y[i] ~ dnorm(mu, 1)
    }
    for(i in 1:5) {
      for(j in 1:5) {
        z[i, j] ~ dnorm(mu, 1)
      }
    }
  })
  inits <- list(mu = 1, y = rnorm(5), z = matrix(rnorm(25), nrow = 5))
  mclass <- nimbleModel::nimbleModel(code, inits = inits, returnClass = TRUE)
  m <- mclass$new()

  nf <- nimbleFunction(
    setup = function(model, nodes) {},
    methods = list(
      get_raw = function(i = integer()) {
        return(values(model, nodes[i]))
        returnType(double(1))
      },
      get_raw_default = function(i = integer()) {
        v <- values(model, nodes[i], mode = "raw")
        return(v)
        returnType(double(1))
      },
      get_scalar = function(i = integer()) {
        v <- 2 * values(model, nodes[i + 1], mode = "scalar")
        return(v)
        returnType(double(1))
      },
      set_raw = function(i = integer(), v = double(1)) {
        values(model, nodes[i]) <<- v
      },
      set_raw_scalar = function(i = integer(), x = double()) {
        values(model, nodes[i]) <<- x
      },
      set_scalar = function(i = integer(), x = double()) {
        values(model, nodes[i], mode = "scalar") <<- x
      }
    ),
    check = FALSE
  )

  # Includes a whole scalar variable and a duplicated entry.
  nodes <- c("y[2:4]", "z[2:4, 3:5]", "mu", "y[3]")
  nf1 <- nf(m, nodes)
  comp <- compileNimble(nf1, m)

  for (obj in list(uncompiled = list(nf = nf1, m = m), compiled = list(nf = comp$nf1, m = comp$m))) {
    mm <- obj$m
    fn <- obj$nf
    entries <- list(mm$y[2:4], as.numeric(mm$z[2:4, 3:5]), mm$mu, mm$y[3])
    for (k in seq_along(nodes)) {
      expect_equal(fn$get_raw(k), entries[[k]], info = nodes[k])
      expect_equal(fn$get_raw_default(k), entries[[k]], info = nodes[k])
    }
    flat <- unlist(entries)
    for (k in seq_along(flat)) {
      expect_equal(fn$get_scalar(k - 1L), 2 * flat[k], info = k)
    }

    new_z <- rnorm(9)
    fn$set_raw(2L, new_z)
    expect_equal(as.numeric(mm$z[2:4, 3:5]), new_z)
    fn$set_raw(4L, 10.5)
    expect_equal(mm$y[3], 10.5)
    fn$set_raw_scalar(3L, -2.5)
    expect_equal(mm$mu, -2.5)
    fn$set_scalar(5L, 7.25) # z[3, 3]
    expect_equal(mm$z[3, 3], 7.25)
    fn$set_scalar(13L, 3.5) # mu
    expect_equal(mm$mu, 3.5)

    expect_error(fn$get_raw(5L))
    expect_error(fn$get_scalar(length(flat)))
    expect_error(fn$set_raw(1L, c(1, 2)))
  }
  comp$nf1 <- comp$m <- NULL
  rm(comp); gc()
})
