# library(nimble2)

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

#library(nimble2)
nimbleOptions(enableDerivs = FALSE)
nCompiler::nOptions(pause_after_writing_files = FALSE)
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

#library(nimble2)
nimbleOptions(enableDerivs = FALSE)
nCompiler::nOptions(pause_after_writing_files = FALSE)
BROWSE_COMPILE_NIMBLE <- FALSE

test_that("model[[node]] works when node is non-scalar", {
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

# KNOWN ISSUE (run with RUN_FAILING_TESTS=1; see test_utils.R).
#
# model[[node]] in run code is compiled to a pointer (scalar node, from
# singleModelIndexAccess_SetupTemplate via make_scalarFieldPtr) or a StridedTensorMap
# (non-scalar node, from map_SetupTemplate via rebind_fieldSTM) into the model variable's
# data, set up once in cpp_init_. If the variable's memory is later replaced, those point
# at freed memory. This happens e.g. when a whole variable of a compiled model is assigned
# from R: nCompiler::value(cm, "y") <- ... moves a new Eigen::Tensor into the model's member
# (the ETaccessor setter does obj = as<ET>(Sinput)). Scalar model variables (plain double
# members, e.g. mu) are not affected. Found 2026-10-06.
#
# Design direction discussed (not yet decided):
#  - Keep the precomputed strides and offsets, which stay valid since a model variable's size
#    and shape don't change, and refresh only the data pointer on each use. In a
#    StridedTensorMap, m_data is the base of the whole source tensor (offsets are in
#    m_startIndices), so a cheap set_data(Scalar*) returning *this would suffice.
#  - In cpp_init_, keep a pointer to the model's member Eigen::Tensor object (whose address is
#    stable even when its buffer moves), e.g. from access(var)->ref<K, double>(), or keep the
#    ETaccessor as multiCopier_model_nC_base does (it refreshes data pointers in setActiveRow).
#  - Refresh on each use, not once at method entry: a method may call code (including R) that
#    replaces a model variable's memory before returning. In run code the replacement must
#    remain a typed expression (not plain nCpp), so it needs the field types from makeFields.
#  - multiCopier_model_nC_base::setActiveRow calls acc->S<double>() per variable per call,
#    which now goes through typed_cast (a string comparison); caching typed pointers in init()
#    would make that cheaper.
RUN_FAILING_TESTS <- FALSE
if(RUN_FAILING_TESTS) {
test_that("model[[node]] still reaches the model variable after its memory is replaced (known failure)", {
  code <- quote({ mu ~ dnorm(0, 1); for(i in 1:3) y[i] ~ dnorm(mu, 1) })
  m <- nimbleModel::nimbleModel(code, inits = list(mu = 0.5), data = list(y = 1:3), returnClass = TRUE)$new()
  fooS <- nimbleFunction(
    setup = function(m_, node) {},
    run = function() { ans <- m_[[node]]; return(ans); returnType(double()) },
    check = FALSE)
  fooV <- nimbleFunction(
    setup = function(m_, node) {},
    run = function() { ans <- m_[[node]]; return(ans); returnType(double(1)) },
    check = FALSE)
  objS <- fooS(m, "y[2]")   # scalar node: pointer case
  objV <- fooV(m, "y[2:3]") # non-scalar node: StridedTensorMap case
  comp <- compileNimble(objS, objV, m)
  expect_equal(comp$objS$run(), 2)
  expect_equal(comp$objV$run(), c(2, 3))
  nCompiler::value(comp$m, "y") <- c(1, 5, 7) # replaces y's memory
  expect_equal(comp$objS$run(), 5)
  expect_equal(comp$objV$run(), c(5, 7))
  rm(comp); gc()
})
}
