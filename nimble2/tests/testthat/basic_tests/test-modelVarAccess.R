library(nimble2)

nimbleOptions(enableDerivs = FALSE)

nCompiler::nOptions(pause_after_writing_files = FALSE)

test_that("model[[node]] when node is a (possibly indexed) scalar works", {
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

  inits <- list(tau = 25, mu = 0.1,
                z = matrix(rnorm(25), nrow = 5))
  data <- list(y = rnorm(5))

  mclass <- nimbleModel::nimbleModel(code, inits = inits, data = data, returnClass = TRUE)
  m <- mclass$new()

  foo <- nimbleFunction(
    setup = function(m_, node) {},
    run = function() {
      ans <- m_[[node]]
      m_[[node]] <<- ans + 1
      return(ans)
      returnType(double())
    },
    check=FALSE
  )
  obj <- foo(m, "y[2]")
  obj2 <- foo(m, "z[3, 2]")
  obj3 <- foo(m, "mu")

  comp <- compileNimble(obj, obj2, obj3, m)
  y2 <- m$y[2]
  z32 <- m$z[3, 2]
  mu <- m$mu
  expect_equal(comp$obj$run(), y2)
  expect_equal(nCompiler::value(comp$m, "y")[2], y2 + 1)
  expect_equal(comp$obj2$run(), z32)
  expect_equal(nCompiler::value(comp$m, "z")[3, 2], z32 + 1)
  expect_equal(comp$obj3$run(), mu)
  expect_equal(nCompiler::value(comp$m, "mu"), mu + 1)

  rm(comp); gc()
})

library(nimble2)
nimbleOptions(enableDerivs = FALSE)
nCompiler::nOptions(pause_after_writing_files = TRUE)
BROWSE_COMPILE_NIMBLE <- FALSE

test_that("model[[node]] when node is non-scalar", {
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

  inits <- list(tau = 25, mu = 0.1,
                z = matrix(rnorm(25), nrow = 5))
  data <- list(y = rnorm(5))

  mclass <- nimbleModel::nimbleModel(code, inits = inits, data = data, returnClass = TRUE)
  m <- mclass$new()

  foo <- nimbleFunction(
    setup = function(m_, node) {},
    run = function() {
      ans <- m_[[node]]
      m_[[node]] <<- ans + 1
      return(ans)
      returnType(double(1))
    },
    check=FALSE
  )
  obj <- foo(m, "y")
  obj2 <- foo(m, "y[2:4]")
  obj3 <- foo(m, "z[1,]")
  obj4 <- foo(m, "z[,2]")
  obj5 <- foo(m, "z[2:4, 2]")
  obj6 <- foo(m, "z[3, 1:3]")

  comp <- compileNimble(obj, obj2, obj3, obj4, obj5, obj6, m)
  y <- m$y
  z <- m$z
  expect_equal(comp$obj$run(), y)
  expect_equal(nCompiler::value(comp$m, "y"), y + 1)
  y <- nCompiler::value(comp$m, "y")
  expect_equal(comp$obj2$run(), y[2:4])
  expect_equal(nCompiler::value(comp$m, "y"), y + c(0, 1, 1, 1, 0))

  expect_equal(comp$obj3$run(), z[1,])
  zAdd <- z
  zAdd[,] <- 0
  zAdd[1,] <- 1
  expect_equal(nCompiler::value(comp$m, "z"), z + zAdd)

  z <- nCompiler::value(comp$m, "z")
  expect_equal(comp$obj4$run(), z[,2])
  zAdd[,] <- 0
  zAdd[,2] <- 1
  expect_equal(nCompiler::value(comp$m, "z"), z + zAdd)

  z <- nCompiler::value(comp$m, "z")
  expect_equal(comp$obj5$run(), z[2:4, 2])
  zAdd[,] <- 0
  zAdd[2:4, 2] <- 1
  expect_equal(nCompiler::value(comp$m, "z"), z + zAdd)

  z <- nCompiler::value(comp$m, "z")
  expect_equal(comp$obj6$run(), z[3, 1:3])
  zAdd[,] <- 0
  zAdd[3, 1:3] <- 1
  expect_equal(nCompiler::value(comp$m, "z"), z + zAdd)

  rm(comp); gc()
})

library(nimble2)
nimbleOptions(enableDerivs = FALSE)
nCompiler::nOptions(pause_after_writing_files = TRUE)
BROWSE_COMPILE_NIMBLE <- FALSE

test_that("model[[node]] when node is non-scalar", {
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
  
  inits <- list(tau = 25, mu = 0.1,
                z = matrix(rnorm(25), nrow = 5))
  data <- list(y = rnorm(5))
  
  mclass <- nimbleModel::nimbleModel(code, inits = inits, data = data, returnClass = TRUE)
  m <- mclass$new()
  
  foo <- nimbleFunction(
    setup = function(m_) {},
    methods = list(
      foo = function() {
        ans <- m_[["y"]]
        m_[["y"]] <<- ans + 1
        return(ans)
        returnType(double(1))
      },
      foo2 = function() {
        ans <- m_[["z[1,]"]]
        m_[["z[,1]"]] <<- ans + 1
        return(ans)
        returnType(double(1))
      },
      foo3 = function() {
        ans <- m_[["mu"]]
        m_[["mu"]] <<- ans + 1
        return(ans)
        returnType(double(0))
      }
    ),
    check=FALSE
  )
  obj <- foo(m)

  comp <- compileNimble(obj, m)
  y <- m$y
  z <- m$z
  mu <- m$mu
  expect_equal(comp$obj$foo(), y)
  expect_equal(nCompiler::value(comp$m, "y"), y + 1)
  y <- nCompiler::value(comp$m, "y")
  
  expect_equal(comp$obj$foo2(), z[1,])
  zCheck <- z
  zCheck[,1] <- zCheck[1,] + 1
  expect_equal(nCompiler::value(comp$m, "z"), zCheck)
  
  expect_equal(comp$obj$foo3(), mu)
  expect_equal(nCompiler::value(comp$m, "mu"), mu + 1)
  
  rm(comp); gc()
})
