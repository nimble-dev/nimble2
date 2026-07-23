library(nimble2)

nimbleOptions(enableDerivs = FALSE)

nCompiler::nOptions(pause_after_writing_files = FALSE)

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
      m_[[node]] <<- ans + 1
      return(ans)
      returnType(double())
    },
    check=FALSE
  )
  obj <- foo(m, "y[2]")
  obj2 <- foo(m, "z[3, 2]")

  comp <- compileNimble(obj, obj2, m)
  y2 <- m$y[2]
  z32 <- m$z[3, 2]
  expect_equal(comp$obj$run(), y2)
  expect_equal(nCompiler::value(comp$m, "y")[2], y2 + 1)
  expect_equal(comp$obj2$run(), z32)
  expect_equal(nCompiler::value(comp$m, "z")[3, 2], z32 + 1)  
})
