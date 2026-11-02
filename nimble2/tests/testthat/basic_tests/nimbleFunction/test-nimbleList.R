
nimbleOptions(enableDerivs = FALSE)
BROWSE_COMPILE_NIMBLE <- FALSE

# debug(nimbleList)
# debug(make_nimbleList_nClass)
# NLdef <- nimbleList(nlScalar = double(0), nlVector = double(1))
# obj <- NLdef$new()
# # Next line gives an error because we must support new(...) in Cpub_class always (evidently)
# obj2 <- NLdef$new(nlScalar = 10, nlVector = 1:3)
# newList2 <- testListDef2$new(nlScalar = doubleMatrix[1,1]*2)
# 
# NLdef <- nimbleList(nlScalar = double(0))
# nCompiler::NCinternals(NLdef)$symbolTable

# scenario 1: nimbleList def and object created in setup code. Not used as a type.
test_that("nimbleList works: scenario 1", {
  nlTestFunc <- nimbleFunction(
    setup = function(){
      testListDef <- nimbleList(nlScalar = double(0), nlVector = double(1))
      newList <- testListDef$new()
      newList$nlVector <- 1:3
    },
    run = function(){
      returnType(numericVector())
      return(newList$nlVector)
    }
  )
  obj <- nlTestFunc()

  comp <- compileNimble(obj)
  expect_equal(comp$run(), 1:3)
  rm(comp); gc()
})

# scenario 2: nimbleList object created in setup code from outside def. Used as returnType.
test_that("nimbleList works: scenario2", {
  testListDef <- nimbleList(nlScalar = double(0), nlVector = double(1))
  nlTestFunc <- nimbleFunction(
    setup = function(){
      newList <- testListDef$new()
      newList$nlVector <- 1:3
    },
    run = function(){
      returnType(testListDef())
      return(newList)
    }
  )
  
  obj <- nlTestFunc()
  comp <- compileNimble(obj)
  expect_equal(comp$run()$nlVector, 1:3)
  rm(testListDef)
  rm(comp); gc()
})

# scenario 3: nimbleList object created in setup code from def created in setup code.
# We need to see the def in the setup code and place it in a scoped location for the nClass

test_that("nimbleList works: scenario 3", {
  nlTestFunc <- nimbleFunction(
    setup = function(){
      testListDef <- nimbleList(nlScalar = double(0), nlVector = double(1))
      v <- 4:6
    },
    run = function(){
      newList <- testListDef$new()
      newList2 <- testListDef$new(nlScalar = sum(v))
      newList$nlVector <- 1:3
      newList$nlScalar <- newList2$nlScalar
      returnType(testListDef())
      return(newList)
    }
  )
  obj <- nlTestFunc()
  comp <- compileNimble(obj)
  
  expect_equal(comp$run()$nlVector, 1:3)
  expect_equal(comp$run()$nlScalar, 15)
})

test_that("nimbleList works: scenario 4", {
  testListDef <- nimbleList(nlScalar = double(0), nlVector = double(1))
  nlTestFunc <- nimbleFunction(
    setup = function(){
      v <- 4:6
    },
    run = function(){
      newList <- testListDef$new()
      newList2 <- testListDef$new(nlScalar = sum(v))
      newList$nlVector <- 1:3
      newList$nlScalar <- newList2$nlScalar
      returnType(testListDef())
      return(newList)
    }
  )
  obj <- nlTestFunc()
  comp <- compileNimble(obj)
  
  expect_equal(comp$run()$nlVector, 1:3)
  expect_equal(comp$run()$nlScalar, 15)
})

# scenario 5: nimbleList object created with initial values in a nimbleFunction without setup code,
# using named and positional arguments.
test_that("nimbleList works: scenario 5", {
  testListDef <- nimbleList(nlScalar = double(0), nlVector = double(1))
  nlTestFunc <- nimbleFunction(
    run = function(x = double(0)) {
      newList <- testListDef$new(nlVector = c(x, 2*x), nlScalar = x + 1)
      newList2 <- testListDef$new(newList$nlScalar, newList$nlVector)
      returnType(testListDef())
      return(newList2)
    }
  )
  comp <- compileNimble(nlTestFunc)
  expect_equal(comp(3)$nlScalar, 4)
  expect_equal(comp(3)$nlVector, c(3, 6))
})

# scenario 6: as scenario 5 but the nimbleFunction without setup code is called from
# a nimbleFunction with setup code.
test_that("nimbleList works: scenario 6", {
  testListDef <- nimbleList(nlScalar = double(0), nlVector = double(1))
  makeList <- nimbleFunction(
    run = function(x = double(0)) {
      newList <- testListDef$new(nlScalar = x, nlVector = c(x, 2*x))
      returnType(testListDef())
      return(newList)
    }
  )
  nlTestFunc <- nimbleFunction(
    setup = function(){
      v <- 4:6
    },
    run = function(){
      returnType(testListDef())
      return(makeList(sum(v)))
    }
  )
  obj <- nlTestFunc()
  comp <- compileNimble(obj)
  expect_equal(comp$run()$nlScalar, 15)
  expect_equal(comp$run()$nlVector, c(15, 30))
})

# scenario 7: nimbleList object created with an initial value that needs keyword processing.
test_that("nimbleList works: scenario 7", {
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
  testListDef <- nimbleList(nlScalar = double(0), nlVector = double(1))
  nlTestFunc <- nimbleFunction(
    setup = function(model, nodes) {},
    run = function() {
      newList <- testListDef$new(nlScalar = model$calculate(nodes))
      returnType(testListDef())
      return(newList)
    },
    check = FALSE
  )
  nf1 <- nlTestFunc(m, nodes)
  cnf <- compileNimble(nf1, m)
  expect_equal(cnf$nf1$run()$nlScalar, m$calculate(nodes))
})
# NULL

# nc1 <- nCompiler::nClass(
#   Cpublic = list(A = 'numericScalar')
# )
# nc2 <- nCompiler::nClass(
#   Cpublic = list(
#     foo = nCompiler::nFunction(
#       function() { obj <- nc1$new()}
#     )
#   )
# )
# comp <- nCompiler::nCompile(nc2)


# nlTestFunc <- nimbleFunction(
#   setup = function(){
#     testListDef <- nimbleList(nlScalar = double(0))
#     doubleMatrix <- diag(1)
#   },
#   run = function(){
#     newList <- testListDef$new(nlScalar = doubleMatrix[1,1]*2)
#     returnType(testListDef())
#     return(newList)
#   }
# )