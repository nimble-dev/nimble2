# library(nimble2)

devtools::load_all()
nimbleOptions(enableDerivs = FALSE)

nCompiler::nOptions(pause_after_writing_files = TRUE)
BROWSE_COMPILE_NIMBLE <- FALSE

test_that("basic modelValues access works", {

  varInfo <- list(
    vars = list(
      mu = list(name = "mu", nDim = 1),
      cov = list(name = "cov", nDim = 2)
    ),
    sizes = list(
      mu = 3,
      cov = c(2, 3)
    )
  )
  mvClass <- nimbleModel:::make_modelValues_nClass(varInfo)
  #CmvClass <- nCompiler::nCompile(mvClass)
  #CmvClass$new()$defaultSizes

  foo <- nimbleFunction(
    setup = function(mvClass) {
      mv <- mvClass$new()
    },
    run = function(i = integer(0)) {
      ans <- mv['mu', i]
      return(ans)
      returnType(double(1))
    },
    methods = list(
      set_mv_mu = function(x = double(1), i = integer(0)) {
        mv['mu', i] <<- x
      }
    ),
    check = FALSE
  )
  foo1 <- foo(mvClass)
#  devtools::load_all()
  #undebug(nCompiler:::new.loadedObjectEnv_full)
  cfoo1 <- compileNimble(foo1)

  derived_class_ID <- nimbleModel:::modelValues(varInfo, .ID = TRUE)
  expect_true(inherits(foo1$mv, "modelValues"))
  expect_true(inherits(foo1$mv, derived_class_ID))

  expect_true(inherits(cfoo1$mv, "modelValues")) # makeTypeObj makes it a base ptr only
  expect_false(inherits(cfoo1$mv, derived_class_ID))

  expect_true(cfoo1$mv$isCompiled())
  expect_equal(cfoo1$mv$getLength(), 0)
  cfoo1$mv$resize(2)
  expect_equal(cfoo1$mv$getLength(), 2)
  expect_equal(nCompiler::value(cfoo1$mv, "mu") |> as.list(), rep(c(0,0,0) |> list(), 2))
  expect_equal(cfoo1$mv["mu", 1], c(0,0,0))
  expect_true(all(c("mu", "cov") %in% nCompiler::interface_names(cfoo1$mv)))

  cfoo1$mv["mu", 1] <- 1:3
  cfoo1$mv["mu", 2] <- 4:6
  expect_equal(cfoo1$mv["mu", 1], 1:3)
  expect_equal(cfoo1$mv["mu", 2], 4:6)
  expect_equal(cfoo1$run(1), 1:3)
  expect_equal(cfoo1$run(2), 4:6)
  cfoo1$set_mv_mu(7:9, 1)
  expect_equal(cfoo1$mv["mu", 1], 7:9)
  cfoo1$set_mv_mu(10:12, 2)
  expect_equal(cfoo1$mv["mu", 2], 10:12)
  rm(cfoo1); gc()
})
