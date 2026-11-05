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
      },
      get_scalar_all = function() { # unindexed scalar mode is raw mode
        return(values(model, nodes, mode = "scalar"))
        returnType(double(1))
      },
      set_scalar_all = function(v = double(1)) {
        values(model, nodes, mode = "scalar") <<- v
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
    expect_equal(fn$get_scalar_all(), values(mm, nodes))
    new_all <- seq_along(flat) + 0.5
    fn$set_scalar_all(new_all)
    # y[3] (entries 2 and 14) is set twice; the last wins, and both entries read it.
    expected_all <- new_all
    expected_all[2] <- new_all[14]
    expect_equal(fn$get_scalar_all(), expected_all)
    expect_equal(mm$y[3], new_all[14])

    expect_error(fn$get_raw(5L))
    expect_error(fn$get_scalar(length(flat)))
    expect_error(fn$set_raw(1L, c(1, 2)))
  }
  comp$nf1 <- comp$m <- NULL
  rm(comp); gc()
})

test_that("values(model, nodes[i], mode = \"node\") works", {
  set.seed(1)
  code <- quote({
    mu ~ dnorm(0, 1)
    for (i in 1:3) {
      y[i] ~ dnorm(mu, 1)
    }
    for (i in 1:2) {
      v[i, 1:3] <- m0[1:3] * i
      W[i, 1:2, 1:2] <- B[1:2, 1:2] * i
    }
  })
  data <- list(y = c(1, 2, 3), m0 = c(1, 2, 3), B = matrix(1:4, 2))
  mclass <- nimbleModel::nimbleModel(code, data = data, inits = list(mu = 0.5), returnClass = TRUE)
  m <- mclass$new()

  nf <- nimbleFunction(
    setup = function(model, nodes) {},
    methods = list(
      get_node = function(i = integer()) {
        v <- values(model, nodes[i], mode = "node")
        return(v)
        returnType(double(1))
      },
      get_node_plus = function(i = integer()) {
        return(2 * values(model, nodes[i + 1], mode = "node"))
        returnType(double(1))
      },
      set_node = function(i = integer(), v = double(1)) {
        values(model, nodes[i], mode = "node") <<- v
      },
      set_node_expr = function(i = integer(), v = double(1)) {
        values(model, nodes[i], mode = "node") <<- 2 * v
      },
      set_node_scalar = function(i = integer(), x = double()) {
        values(model, nodes[i], mode = "node") <<- x
      },
      get_all = function() { # unindexed node mode: all the nodes, in the same order
        return(values(model, nodes, mode = "node"))
        returnType(double(1))
      },
      set_all = function(v = double(1)) {
        values(model, nodes, mode = "node") <<- v
      }
    ),
    check = FALSE
  )

  # Nodes are in sorted order (as for calculate(model, nodes[i])), with data nodes included.
  # nimbleModel's test-valuesNode.R checks which node is which, so here the reference is
  # nimbleModel::values_indexed on the same model.
  nodes <- c("W", "y[2:3]", "mu")
  nf1 <- nf(m, nodes)
  comp <- compileNimble(nf1, m)

  for (obj in list(uncompiled = list(nf = nf1, m = m), compiled = list(nf = comp$nf1, m = comp$m))) {
    mm <- obj$m
    fn <- obj$nf
    mm$calculate()
    n <- nimbleModel::makeInstrList(mm, nodes)$nNodes_total
    expect_equal(n, 5)
    ref <- lapply(seq_len(n), \(k) nimbleModel::values_indexed(mm, nodes, k, "node"))
    lens <- lengths(ref)
    expect_equal(sort(lens), c(1, 1, 1, 4, 4))
    expect_equal(ref[[which(lens == 1)[1]]], mm$mu) # mu sorts before y
    for (k in seq_len(n)) {
      expect_equal(fn$get_node(k), ref[[k]], info = k)
    }
    for (k in 2:n) {
      expect_equal(fn$get_node_plus(k - 1L), 2 * ref[[k]], info = k) # get_node_plus(i) uses nodes[i + 1]
    }
    for (k in seq_len(n)) {
      newv <- seq_len(lens[k]) + 10 * k
      fn$set_node(k, newv)
      expect_equal(nimbleModel::values_indexed(mm, nodes, k, "node"), newv, info = k)
      fn$set_node_expr(k, newv)
      expect_equal(nimbleModel::values_indexed(mm, nodes, k, "node"), 2 * newv, info = k)
    }
    k1 <- which(lens == 1)[2]
    fn$set_node_scalar(k1, 9.5)
    expect_equal(nimbleModel::values_indexed(mm, nodes, k1, "node"), 9.5)
    k4 <- which(lens == 4)[1]

    expect_error(fn$get_node(n + 1L))
    expect_error(fn$set_node(k4, c(1, 2)))
    expect_error(fn$set_node_scalar(k4, 1))

    allRef <- unlist(lapply(seq_len(n), \(k) nimbleModel::values_indexed(mm, nodes, k, "node")))
    expect_equal(fn$get_all(), allRef)
    newAll <- seq_along(allRef) + 0.25
    fn$set_all(newAll)
    expect_equal(fn$get_all(), newAll)
    expect_equal(unlist(lapply(seq_len(n), \(k) fn$get_node(k))), newAll)
    expect_error(fn$set_all(newAll[-1]), "length")
    expect_equal(fn$get_all(), newAll) # unchanged by the failed set
  }
  comp$nf1 <- comp$m <- NULL
  rm(comp); gc()

  # Setup errors: RHS-only elements and logProb variables are not allowed in node mode.
  # Keyword setup code runs in compileNimble, so the errors come from there.
  m2 <- mclass$new()
  expect_error(compileNimble(nf(m2, c("mu", "m0[1:2]")), m2), "RHS-only")
  expect_error(compileNimble(nf(m2, c("mu", "logProb_y[1:2]")), m2), "logProb")
})

test_that("values(model, nodes[i], mode = \"node\") shares its instrList with calculate(nodes[i])", {
  code <- quote({
    mu ~ dnorm(0, 1)
    for (i in 1:3) {
      y[i] ~ dnorm(mu, 1)
    }
  })
  mclass <- nimbleModel::nimbleModel(code, data = list(y = c(1, 2, 3)), inits = list(mu = 0.5), returnClass = TRUE)
  m <- mclass$new()
  # calculate comes first, so its setup code (without the values check) is added first.
  nf <- nimbleFunction(
    setup = function(model, nodes) {},
    methods = list(
      lp = function(i = integer()) {
        return(model$calculate(nodes[i]))
        returnType(double())
      },
      get1 = function(i = integer()) {
        return(values(model, nodes[i], mode = "node"))
        returnType(double(1))
      },
      get2 = function(i = integer()) {
        return(2 * values(model, nodes[i], mode = "node"))
        returnType(double(1))
      },
      getAll = function() {
        return(values(model, nodes, mode = "node"))
        returnType(double(1))
      }
    ),
    check = FALSE
  )
  nodes <- c("y", "mu")
  nf1 <- nf(m, nodes)
  comp <- compileNimble(nf1, m)
  # One instrList, shared by all four calls. The check makes no field.
  fieldNames <- ls(comp$nf1)
  expect_equal(sum(grepl("instrList", fieldNames)), 1)
  expect_equal(sum(grepl("valuesNode_checked", fieldNames)), 0)
  # Node i is the same for calculate and values: sorted order is mu, y[1], y[2], y[3].
  cm <- comp$m
  for (k in 1:4) {
    expect_equal(comp$nf1$get1(k), nimbleModel::values_indexed(cm, nodes, k, "node"))
    expect_equal(comp$nf1$get2(k), 2 * comp$nf1$get1(k))
    expect_equal(comp$nf1$lp(k), cm$calculate_indexed(nodes, k))
  }
  expect_equal(comp$nf1$get1(1), cm$mu)
  expect_equal(comp$nf1$getAll(), unlist(lapply(1:4, \(k) comp$nf1$get1(k))))
  comp$nf1 <- comp$m <- NULL
  rm(comp); gc()

  # The check still runs when calculate(nodes[i]) has already added the instrList.
  code2 <- quote({
    mu ~ dnorm(0, 1)
    for (i in 1:3) {
      y[i] ~ dnorm(mu + x[i], 1)
    }
  })
  m2 <- nimbleModel::nimbleModel(code2, data = list(y = c(1, 2, 3), x = c(0, 0, 0)), inits = list(mu = 0.5))
  expect_error(compileNimble(nf(m2, c("mu", "x[1:2]")), m2), "RHS-only")
})
