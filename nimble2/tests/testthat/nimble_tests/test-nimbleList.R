## Tests for building, compiling, and using nimbleList objects
## There are five distinct ways that a nimbleList can be created for use in a nimbleFunction:
## 1) nimbleListDef is created outside of the nimbleFunction, nimbleListDef$new() used to create new nimbleList in setup code
## 2) nimbleListDef is created outside of the nimbleFunction, nimbleListDef$new() used to create new nimbleList in run code
## 3) nimbleListDef is created in setup code of the nimbleFunction, nimbleListDef$new() used to create new nimbleList in setup code
## 4) nimbleListDef is created in setup code of the nimbleFunction, nimbleListDef$new() used to create new nimbleList in run code
## 5) nimbleListDef is created outside of the nimbleFunction, nimbleListDef$new() used to create new nimbleList outside of the nimbleFunction,
##    nimbleList passed as argument to nimbleFunction

source(system.file(file.path('tests', 'testthat', 'test_utils.R'), package = 'nimble'))

RwarnLevel <- options('warn')$warn
options(warn = 1)
nimbleVerboseSetting <- nimbleOptions('verbose')
nimbleOptions(verbose = FALSE)

######## (works in nimble2)
## Test of creating new nimbleList in run code and specifying initial values for that list
## Here, the nlDef is created in the global environment, outside of setup code
########
test_that("nimbleList test 1: return objects are nimbleLists", {
  testListDef1 <- nimbleList(nlScalar = double(0), nlVector = double(1), nlMatrix = double(2))
  # temporarilyAssignInGlobalEnv(testListDef1)
  nlTestFunc1 <- nimbleFunction(
    setup = function(){
      doubleMatrix <- diag(1)
    },
    run = function(){
      doubleScalar <- 1
      doubleVector <- numeric(2, 1)
      newList1 <- testListDef1$new(nlVector = doubleVector, nlMatrix = doubleMatrix)
      newList1$nlScalar <- doubleScalar
      returnType(testListDef1())
      return(newList1)
    }
  )

  testInst <- nlTestFunc1()
  RnimbleList <- testInst$run()
  CtestInst <- compileNimble(testInst)
  CnimbleList <- CtestInst$run()
  
  ## test for correct values of R nimbleList
  expect_identical(RnimbleList$nlScalar, 1)
  expect_identical(RnimbleList$nlVector, c(1, 1))
  expect_identical(RnimbleList$nlMatrix, diag(1))
  ## test for identical values of R and C nimbleLists
  expect_identical(RnimbleList$nlScalar, CnimbleList$nlScalar)
  expect_identical(RnimbleList$nlVector, CnimbleList$nlVector)
  expect_identical(RnimbleList$nlMatrix, CnimbleList$nlMatrix)
  #
  expect_true(is.nl(RnimbleList))
  expect_true(is.nl(CnimbleList))
  rm(RnimbleList, CnimbleList); gc()
})


######## (works in nimble2)
## Test of creating new nimbleList in run code and specifying initial value for that list
## Here, the nlDef is created in setup code.  Inital value is an expression.
########
test_that("nimbleList test 2: return objects are nimbleLists", {
  nlTestFunc2 <- nimbleFunction(
    setup = function(){
      testListDef2 <- nimbleList(nlScalar = double(0))
      doubleMatrix <- diag(1)
    },
    run = function(){
      newList2 <- testListDef2$new(nlScalar = doubleMatrix[1,1]*2)
      returnType(testListDef2())
      return(newList2)
    }
  )
  
  testInst <- nlTestFunc2()
  RnimbleList <- testInst$run()
  CtestInst <- compileNimble(testInst, control = list(debug =  F))
  CnimbleList <- CtestInst$run()
  
  ## test for correct values of R nimbleList
  expect_identical(RnimbleList$nlScalar, 2)
  ## test for identical values of R and C nimbleLists
  expect_identical(RnimbleList$nlScalar, CnimbleList$nlScalar)
  expect_identical(is.nl(RnimbleList), TRUE)
  expect_identical(is.nl(CnimbleList), TRUE)
})

######## (works in nimble2)
## Test of creating new nimbleList in setup code and specifying initial values for that list.
## Here, the nlDef is created in setup code
########
test_that("nimbleList test 3: return objects are nimbleLists", {
  nlTestFunc3 <- nimbleFunction(
    setup = function(){
      testTypes <- list(nimbleType(name = 'nlCharacter', type = 'character', dim = 0))
      testListDef3 <- nimbleList(testTypes)
      setupList3 <- testListDef3$new(nlCharacter = "hello world")
    },
    run = function(){
      returnType(testListDef3())
      return(setupList3)
    }
  )
  
  testInst <- nlTestFunc3()
  RnimbleList <- testInst$run()
  CtestInst <- compileNimble(testInst)
  CnimbleList <- CtestInst$run()
  
  ## test for correct values of R nimbleList
  expect_identical(RnimbleList$nlCharacter, "hello world")
  ## test for identical values of R and C nimbleLists
  expect_identical(RnimbleList$nlCharacter, CnimbleList$nlCharacter)
  expect_identical(CnimbleList$nlCharacter, CtestInst$setupList3$nlCharacter)
  
  expect_identical(is.nl(RnimbleList), TRUE)
  expect_identical(is.nl(CnimbleList), TRUE)
  # expect_identical(is.nl(CtestInst$setupList3), TRUE)  is.nl gives FALSE here, not sure if should be corrected
})

# # NL2FIXME (character vector needed)
# ########
# ## Test of using a character vector in a nimbleList.  nimbleList created in setup code
# ########
# test_that("nimbleList test 3a: return objects are nimbleLists", {
#   nlTestFunc3a <- nimbleFunction(
#     setup = function(){
#       testTypes <- list(nimbleType(name = 'nlCharacters', type = 'character', dim = 1))
#       testListDef3a <- nimbleList(testTypes)
#       setupList3a <- testListDef3a$new(nlCharacters = c("hello", "world"))
#     },
#     run = function(){
#       returnType(testListDef3a())
#       return(setupList3a)
#     }
#   )
#   
#   testInst <- nlTestFunc3a()
#   RnimbleList <- testInst$run()
#   CtestInst <- compileNimble(testInst)
#   CnimbleList <- CtestInst$run()
#   
#   ## test for correct values of R nimbleList
#   expect_identical(RnimbleList$nlCharacters[1], "hello")
#   expect_identical(RnimbleList$nlCharacters[2], "world")
#   ## test for identical values of R and C nimbleLists
#   expect_identical(RnimbleList$nlCharacters, CnimbleList$nlCharacters)
#   expect_identical(CnimbleList$nlCharacters, CtestInst$setupList3a$nlCharacters)
#   expect_identical(nimble:::is.nl(RnimbleList), TRUE)
#   expect_identical(is.nl(CnimbleList), TRUE)
# })

# # NL2FIXME (character vector needed)
# ######## 
# ## Test of using a character vector in a nimbleList.  nimbleList created in run code
# ########
# test_that("nimbleList test 3b: return objects are nimbleLists", {
#   nlTestFunc3b <- nimbleFunction(
#     setup = function(){
#       testTypes <- list(nimbleType(name = 'nlCharacters', type = 'character', dim = 1))
#       testListDef3b <- nimbleList(testTypes)
#       charVec <- c('hello', 'world')
#     },
#     run = function(){
#       runList3b <- testListDef3b$new(nlCharacters = charVec)
#       returnType(testListDef3b())
#       return(runList3b)
#     }
#   )
#   
#   testInst <- nlTestFunc3b()
#   RnimbleList <- testInst$run()
#   CtestInst <- compileNimble(testInst)
#   CnimbleList <- CtestInst$run()
#   
#   ## test for correct values of R nimbleList
#   expect_identical(RnimbleList$nlCharacters[1], "hello")
#   expect_identical(RnimbleList$nlCharacters[2], "world")
#   ## test for identical values of R and C nimbleLists
#   expect_identical(RnimbleList$nlCharacters, CnimbleList$nlCharacters)
#   expect_identical(nimble:::is.nl(RnimbleList), TRUE)
#   expect_identical(is.nl(CnimbleList), TRUE)
#   ## expect_identical(is.nl(CtestInst$setupList3), TRUE)  is.nl gives FALSE here, not sure if should be corrected
# })



######## (works in nimble2)
## Test of creating new nimbleList in a function that is internal to another function.
## The list is passed from the internal function to the outer function, and then to the user.
## This test does not use nimble's active binding system.
########
test_that("nimbleList test 4: return objects are nimbleLists", {
  testListDef4 <- nimbleList(list(nimbleType(name = 'nlCharacter', type = 'character', dim = 0)))
#  temporarilyAssignInGlobalEnv(testListDef4)
  
  innerNlTestFunc4 <- nimbleFunction(
    setup = function(){
      setupList4 <- testListDef4$new(nlCharacter = "hello world")
    },
    run = function(){
      returnType(testListDef4())
      return(setupList4)
    }
  )
#  temporarilyAssignInGlobalEnv(innerNlTestFunc4)
  
  nlTestFunc4 <- nimbleFunction(
    setup = function(){
      innerFunc4 <- innerNlTestFunc4()
    },
    run = function(){
      outList <- innerFunc4$run()
      returnType(testListDef4())
      return(outList)
    }
  )
  
  testInst <- nlTestFunc4()
  RnimbleList <- testInst$run()
  CtestInst <- compileNimble(testInst)
  CnimbleList <- CtestInst$run()
  
  ## test for correct values of R nimbleList
  expect_identical(RnimbleList$nlCharacter, "hello world")
  ## test for identical values of R and C nimbleLists
  expect_identical(RnimbleList$nlCharacter, CnimbleList$nlCharacter)
  expect_identical(is.nl(RnimbleList), TRUE)
  expect_identical(is.nl(CnimbleList), TRUE)
})

######## (works in nimble2)
## Test of creating and interacting with two nimble lists in two different functions from the same def'n
########
test_that("nimbleList test 5: return objects are nimbleLists", {
  testListDef5 <- nimbleList(nlCharacter = character(0))
  #temporarilyAssignInGlobalEnv(testListDef5)
  
  innerNlTestFunc5 <- nimbleFunction(
    setup = function(listDef){
      innerSetupList5 <- listDef$new(nlCharacter = "hello world")
    },
    run = function(){
      returnType(testListDef5())
      return(innerSetupList5)
    }
  )
  #temporarilyAssignInGlobalEnv(innerNlTestFunc5)
  
  nlTestFunc5 <- nimbleFunction(
    setup = function(listDef){
      innerFunc5 <- innerNlTestFunc5(listDef)
      outerSetupList5 <- listDef$new(nlCharacter = "goodbye!")
    },
    run = function(){
      innerNimList <- innerFunc5$run()
      innerNimList <- outerSetupList5
      returnType(testListDef5())
      return(innerNimList)
    }
  )
  
  testInst <- nlTestFunc5(testListDef5)
  RnimbleList <- testInst$run()
  CtestInst <- compileNimble(testInst)
  CnimbleList <- CtestInst$run()
  
  ## test for correct values of R nimbleList
  expect_identical(RnimbleList$nlCharacter, "goodbye!")
  ## test for identical values of R and C nimbleLists
  expect_identical(RnimbleList$nlCharacter, CnimbleList$nlCharacter)
  expect_identical(is.nl(RnimbleList), TRUE)
  expect_identical(is.nl(CnimbleList), TRUE)
})

######## (works in nimble2)
## Test of creating an nlDef in an outer function, passing the def to the inner function,
## and returning the created list from the inner function through the outer function
########
test_that("nimbleList test 6: return objects are nimbleLists", {
  innerNlTestFunc6 <- nimbleFunction(
    setup = function(nimListDef){
      setupList6 <- nimListDef$new(nlCharacter = "hello world")
    },
    run = function(){
      returnType(nimListDef())
      return(setupList6)
    }
  )
  
  #temporarilyAssignInGlobalEnv(innerNlTestFunc6)
  nlTestFunc6 <- nimbleFunction(
    setup = function(){
      testTypes <- list(nimbleType('nlCharacter', 'character', 0))
      testListDef6 <- nimbleList(testTypes)
      innerFunc6 <- innerNlTestFunc6(testListDef6)
    },
    run = function(){
      outList <- innerFunc6$run()
      returnType(testListDef6())
      return(outList)
    }
  )
  
  testInst <- nlTestFunc6()
  RnimbleList <- testInst$run()
  CtestInst <- compileNimble(testInst)
  CnimbleList <- CtestInst$run()
  
  ## test for correct values of R nimbleList
  expect_identical(RnimbleList$nlCharacter, "hello world")
  ## test for identical values of R and C nimbleLists
  expect_identical(RnimbleList$nlCharacter, CnimbleList$nlCharacter)
  expect_identical(is.nl(RnimbleList), TRUE)
  expect_identical(is.nl(CnimbleList), TRUE)
})
