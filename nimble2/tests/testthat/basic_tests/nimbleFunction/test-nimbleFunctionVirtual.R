nimbleOptions(enableDerivs = FALSE)
BROWSE_COMPILE_NIMBLE <- FALSE

test_that("nimbleFunctionVirtual works", {
  baseNF <- nimbleFunctionVirtual(
    run = function(x = double()) {
      returnType(double())
    },
    methods = list(
      foo = function(x = double(1)) {
        returnType(double(1))
      },
      hw = function(x = double(1)) {
        return(x + 1)
        returnType(double(1))
      }
    ),
    methodControl = list(hw = list(required = FALSE))
  )
  
  dNFa <- nimbleFunction(
    contains = baseNF,
    setup = TRUE,
    run = function(x = double()) {
      return(x + 2)
      returnType(double())
    },
    methods = list(
      foo = function(x = double(1)) {
        return(x + 3)
        returnType(double(1))
      },
      hw = function(x = double(1)) {
        return(x + 4)
        returnType(double(1))
      }
    )
  )
  
  dNFb <- nimbleFunction(
    contains = baseNF,
    setup = TRUE,
    run = function(x = double()) {
      return(x + 2)
      returnType(double())
    },
    methods = list(
      foo = function(x = double(1)) {
        return(x + 3)
        returnType(double(1))
      } # NO hw method provided.
    )
  )
  
  obj_a <- dNFa()
  obj_b <- dNFb()
  comp <- compileNimble(obj_a, obj_b)  
  
  obj <- comp$obj_a
  expect_equal(obj$run(1), 3)
  expect_equal(obj$foo(1:3), 4:6)
  expect_equal(obj$hw(1:3), 5:7)
  obj <- obj_a
  expect_equal(obj$run(1), 3)
  expect_equal(obj$foo(1:3), 4:6)
  expect_equal(obj$hw(1:3), 5:7)
  
  obj <- comp$obj_b
  expect_equal(obj$run(1), 3)
  expect_equal(obj$foo(1:3), 4:6)
  expect_equal(obj$hw(1:3), 2:4) # base class hw
  obj <- obj_b
  expect_equal(obj$run(1), 3)
  expect_equal(obj$foo(1:3), 4:6)
  expect_equal(obj$hw(1:3), 2:4) # base class hw
  
  ## message capturing in testthat is a pain
  ## Check for warnings: foo was required but not provided
  res <- capture_messages(dNFc <- nimbleFunction(
    contains = baseNF,
    setup = TRUE,
    run = function(x = double()) {
      return(x + 2)
      returnType(double())
    }
  ))
  expect_true(grepl("[Warning]", res))
  
  ## Check for no warning: run is required but defaults to function(){}, so no warning is issued
  res <- capture_messages(
    dNFc <- nimbleFunction(
      contains = baseNF,
      setup = TRUE,
      methods = list(
        foo = function(x = double(1)) {
          return(x + 2)
          returnType(double(1))
        } # NO hw method provided, but not required
      )
    )
  )
  expect_identical(res, character())
  
  useBase <- nimbleFunction(
    setup = function() {
      NFlist <- nimbleFunctionList(baseNF)
      NFlist[[1]] <- dNFa()
      NFlist[[2]] <- dNFb()
    },
    run = function(x = double(1), i = integer(0)) {
      x[1] <- NFlist[[i]]$run(x[1])
      x <- NFlist[[i]]$foo(x)
      x <- NFlist[[i]]$hw(x)
      return(x)
      returnType(double(1))
    }
  )

  useBase1 <- useBase()
  CuseBase1 <- compileNimble(useBase1)
  Cans <- CuseBase1$run(c(10, 100), 1)
  expect_equal(Cans, c(10, 100) + c(9, 7))
  ans <- useBase1$run(c(10, 100), 1)
  expect_equal(ans, Cans)

  Cans <- CuseBase1$run(c(10, 100), 2)
  expect_equal(Cans, c(10, 100) + c(6, 4))
  ans <- useBase1$run(c(10, 100), 2)
  expect_equal(ans, Cans)
  
  rm(useBase1, CuseBase1); gc()    
})
