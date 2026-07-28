library(nimble2)

devtools::load_all()
nimbleOptions(enableDerivs = FALSE)

nCompiler::nOptions(pause_after_writing_files = TRUE)

test_that("basic modelValues acces works", {
  
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
  
  foo <- nimbleFunction(
    setup = function(mvClass) {
      mv <- mvClass$new()
    },
    run = function() {
      ans <- mv['mu', 1]
      return(ans)
      returnType(double(1))
    },
    check = FALSE
  )
  foo1 <- foo(mvClass)
#  devtools::load_all()
  cfoo1 <- compileNimble(foo1)
    
})