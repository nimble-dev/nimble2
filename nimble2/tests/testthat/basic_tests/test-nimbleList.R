

debug(nimbleList)
debug(make_nimbleList_nClass)
NLdef <- nimbleList(nlScalar = double(0), nlVector = double(1))
obj <- NLdef$new()
# Next line gives an error because we must support new(...) in Cpub_class always (evidently)
obj2 <- NLdef$new(nlScalar = 10, nlVector = 1:3)
newList2 <- testListDef2$new(nlScalar = doubleMatrix[1,1]*2)

NLdef <- nimbleList(nlScalar = double(0))
nCompiler::NCinternals(NLdef)$symbolTable

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
  nimbleOptions(enableDerivs = FALSE)
  BROWSE_COMPILE_NIMBLE <- TRUE
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
  nimbleOptions(enableDerivs = FALSE)
  BROWSE_COMPILE_NIMBLE <- TRUE
  #undebug(nCompiler:::labelAbstractTypesEnv$DollarSign)
  comp <- compileNimble(obj)
  expect_equal(comp$run()$nlVector, 1:3)
  rm(testListDef)
  rm(comp); gc()
})

# scenario 3: nimbleList object created in setup code from def created in setup code.
# We need to see the def in the setup code and place it in a scoped location for the nClass

devtools::load_all()
nlTestFunc <- nimbleFunction(
  setup = function(){
    testListDef <- nimbleList(nlScalar = double(0), nlVector = double(1))
  },
  run = function(){
    newList <- testListDef$new(nlScalar = 4)
    newList$nlVector <- 1:3
    returnType(testListDef())
    return(newList)
  }
)
obj <- nlTestFunc()
nimbleOptions(enableDerivs = FALSE)
BROWSE_COMPILE_NIMBLE <- FALSE
#undebug(nCompiler:::labelAbstractTypesEnv$DollarSign)
#debug(nCompiler:::resolveOneTBDsymbol)
#debug(nCompiler:::labelAbstractTypesEnv$AssignAfterRecursing)
#undebug(nCompiler:::compile_labelAbstractTypes)
debug(nCompiler:::genCppEnv$ChainedCall)
debug(nCompiler:::genCppEnv$LambdaFun_)
nCompiler::nOptions(pause_after_writing_files = TRUE)
comp <- compileNimble(obj)

comp$run()$nlVector
comp$run()$nlScalar

# NULL

nc1 <- nCompiler::nClass(
  Cpublic = list(A = 'numericScalar')
)
nc2 <- nCompiler::nClass(
  Cpublic = list(
    foo = nCompiler::nFunction(
      function() { obj <- nc1$new()}
    )
  )
)
comp <- nCompiler::nCompile(nc2)


nlTestFunc <- nimbleFunction(
  setup = function(){
    testListDef <- nimbleList(nlScalar = double(0))
    doubleMatrix <- diag(1)
  },
  run = function(){
    newList <- testListDef$new(nlScalar = doubleMatrix[1,1]*2)
    returnType(testListDef())
    return(newList)
  }
)