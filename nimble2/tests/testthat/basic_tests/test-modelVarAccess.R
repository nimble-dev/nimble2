library(nimble2)

nimbleOptions(enableDerivs = FALSE)

nCompiler::nOptions(pause_after_writing_files = TRUE)

test_that("model[[node]] works", {
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

  inits <- list(tau = 25, mu = 0,
                z = matrix(rnorm(25), nrow = 5))
  data <- list(y = rnorm(5))

  mclass <- nimbleModel::nimbleModel(code, inits = inits, data = data, returnClass = TRUE)
  m <- mclass$new()

  foo <- nimbleFunction(
    setup = function(m_, node) {},
    run = function() {
      ans <- m_[[node]]
      return(ans)
      returnType(double())
    },
    check=FALSE
  )
  obj <- foo(m, "y[2]")
  undebug(nimble2:::determineNdimFromOneCase)
  cobj <- compileNimble(obj)
})
