library(testthat)
library(nimble2)
nimbleOptions(enableDerivs = FALSE)

test_that("math functions in model code give the same results compiled and uncompiled", {
  code <- quote({
    a ~ dnorm(0, 1)
    b1 <- step(a) + equals(a, 0.5) + pow(a, 2) + inprod(v[1:2], v[1:2])
    b2 <- expit(a) + ilogit(a) + logit(0.3) + probit(0.3) + iprobit(a) + phi(a) +
      cloglog(0.3) + icloglog(a) + loggam(2.5) + lfactorial(3) + cube(a)
    d <- logdet(S[1:2, 1:2])
    Sinv[1:2, 1:2] <- inverse(S[1:2, 1:2])
    v[1:2] ~ dmnorm(mu[1:2], cov = S[1:2, 1:2])
    logit(p) <- a
    y ~ dbern(p)
  })
  S <- matrix(c(2, 0.5, 0.5, 1), 2)
  mclass <- nimbleModel::nimbleModel(code,
    constants = list(mu = c(0, 0)), data = list(y = 1, S = S),
    inits = list(a = 0.5, v = c(1, 2)), returnClass = TRUE
  )
  m <- mclass$new()
  nf <- nimbleFunction(
    setup = function(model) {},
    run = function() {
      model$calculate()
    },
    check = FALSE
  )
  nf1 <- nf(m)
  cnf <- compileNimble(nf1, m)
  for (a in c(-1, 0.5, 2)) {
    m$a <- a
    cnf$m$a <- a
    expect_equal(cnf$m$calculate(), m$calculate())
    for (v in c("b1", "b2", "d", "Sinv", "p")) {
      expect_equal(cnf$m[[v]], m[[v]])
    }
  }
  expect_equal(cnf$m$Sinv, solve(S))
  expect_equal(cnf$m$d, log(det(S)))
})
