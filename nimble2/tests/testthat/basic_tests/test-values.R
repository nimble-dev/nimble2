library(testthat)
library(nimble2)
nimbleOptions(enableDerivs = FALSE)

nCompiler::nOptions(pause_after_writing_files = TRUE)

BROWSE_COMPILE_NIMBLE <- FALSE

test_that("values(mode, nodes) works", {
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

  nf <- nimbleFunction(
    setup = function(model, nodes) {},
    run = function() {
        v <- values(model, nodes)
        return(v)
        returnType(double(1))
    },
    check = FALSE
  )
  nf1 <- nf(m, list(nimbleModel::varRangeClass$new("y[1:3]")))
  ## debug(nimble2:::values_keywordInfo$processor)
  ## debug(nimbleModel::makeMultiCopier)
  comp <- compileNimble(nf1, m)

 expect_equal( comp$m$y[1:3],
              comp$nf1$run())
})
