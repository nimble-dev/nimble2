library(testthat)
library(nimble2)
nimbleOptions(enableDerivs = FALSE)

BROWSE_COMPILE_NIMBLE <- TRUE

test_that("nimbleFunction / RCfunction works and compiles through nCompile", {
  foo <- nimbleFunction(
    run = function(x = double(1)) {
      y <- x + 1
      return(y)
      returnType(double(1))
    },
    check=FALSE
  )
  foo(1:3)
  cfoo <- nCompiler::nCompile(foo) # eventually needs to go through nimble2::compileNimble
  expect_equal(cfoo(1:3), 2:4)
  # need nimbleOptions call above first:
  cfoo <- compileNimble(foo)#, control = list(nCompiler_expandUnits = FALSE))
  expect_equal(cfoo(1:3), 2:4)  
})

devtools::load_all()

nimbleOptions(enableDerivs = FALSE)

BROWSE_COMPILE_NIMBLE <- TRUE

nf <- nimbleFunction(
  setup = function() {x <- 1:3},
  run = function(y = double(1)) {
    ans <- x + y
    return(ans)
    returnType(double(1))
  },
  check=FALSE
)
#expect_false(is.nf(nf))
#expect_true(is.nfGenerator(nf))
nf1 <- nf()
#expect_true(is.nf(nf1))
#expect_equal(nf1$run(1:3), 2*(1:3))

cnf <- compileNimble(nf1)

#### 
## side experiment on nCompiler types
## Result: confirmed that a list can have a symbol object directly in place for a type
make_types <- function() {
  my_sym <- nCompiler:::type2symbol('numericVector')
  list(x = my_sym,
       y = 'integerScalar')
}
nc <- eval(substitute(nCompiler::nClass(Cpublic = CPUBLIC), list(CPUBLIC = make_types())))
