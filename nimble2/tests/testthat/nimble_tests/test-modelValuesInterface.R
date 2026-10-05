source(system.file(file.path('tests', 'testthat', 'test_utils.R'), package = 'nimble'))

RwarnLevel <- options('warn')$warn
options(warn = 1)
nimbleVerboseSetting <- nimbleOptions('verbose')
nimbleOptions(verbose = FALSE)

# works in nimble2
test_that("copying of argument passing in nimbleFunctionInterface", {
  mvc <- modelValuesConf(vars = c('d1'),
                         types = c('double'),
                         sizes = list( d1 = 5 ))

  mv <- mvc(2)

  nfG <- nimbleFunction(
    setup = function(mv) {},
    run = function() {
      mv['d1', 1] <<- rnorm(5)
      ans <- mv['d1', 1]
      return(ans)
      returnType(double(1))
    }
  )
  nf <- nfG(mv)
  nf$run()

  cnf <- compileNimble(nf)
  cnf$run()

  cnf$mv['d1',1] <- as.numeric(1:5)
  ans <- cnf$mv['d1',1]

  expect_identical(ans, as.numeric(1:5))
  remove(cnf, mvc, mv, nfG); gc()
})

# works in nimble2 with test modifications
# nimble2 modifications: use some as.list() of nLists for comparing equality
test_that("as.matrix and matrix2mv work for modelValues", {
  mvc <- modelValuesConf(vars = c('d1', 'd2', 'd3'),
                         types = c('double', 'double', 'double'),
                         sizes = list( d1 = 5, d2 = c(3, 5), d3 = c(3, 5, 7)))

  mv <- mvc(3)

  nfG <- nimbleFunction(
    setup = function(mv) {
      setupOutputs(mv)
    },
    run = function() {}
  )
  nf <- nfG(mv)
  cnf <- compileNimble(nf)

  for(i in 1:3) {
    mv$d1[[i]] <- rnorm(5)
    mv$d2[[i]] <- matrix(rnorm(15), nrow = 3)
    mv$d3[[i]] <- array(rnorm(3*5*7), dim = c(3, 5, 7))
  }
  ## check assignment by variable
  # Note that whwn we access cnf$mv, we are getting a base class interface.
  # Use of `[` works because it uses value()
  cnf$mv['d1'] <- mv['d1']
  expect_equal(cnf$mv['d1'] |> as.list(), mv['d1'] |> as.list())
  cnf$mv['d2'] <- mv['d2']
  expect_equal(cnf$mv['d2'] |> as.list(), mv['d2'] |> as.list())
  cnf$mv['d3'] <- mv['d3']
  expect_equal(cnf$mv['d3'] |> as.list(), mv['d3'] |> as.list())

  expect_true(is.null(cnf$mv$d1)) # because it is a base class interface, it lacks active bindings to the derived names

    ## get new values and check that as.matrix and matrix2mv are inverses
  for(i in 1:3) {
    mv$d1[[i]] <- rnorm(5)
    mv$d2[[i]] <- matrix(rnorm(15), nrow = 3)
    mv$d3[[i]] <- array(rnorm(3*5*7), dim = c(3, 5, 7))
  }
  nimbleModel::matrix2mv(as.matrix(mv), cnf$mv)
  expect_equal(cnf$mv['d1'] |> as.list(), mv['d1'] |> as.list())
  expect_equal(cnf$mv['d2'] |> as.list(), mv['d2'] |> as.list())
  expect_equal(cnf$mv['d3'] |> as.list(), mv['d3'] |> as.list())
  newMat <- as.matrix(cnf$mv)
  expect_equal(as.matrix(mv), newMat)
}
)

# N2FIXME: This test requires MCMC
# test_that("as.list works for CmodelValues", {
#   mc <- nimbleModel:::nimbleCode({
#     for(i in 1:3) x[i] ~ dnorm(0,1)
#     for(i in 1:3)
#       for(j in 1:2) y[i, j] ~ dnorm(0,1)
#     for(i in 1:3)
#       for(j in 1:2)
#         for(k in 1:4)
#           z[i, j, k ] ~ dnorm(0,1)
#   })
# 
#   m <- nimbleModel::nimbleModel(mc)
#   buildMCMC(m)
# 
#   ## uncompiled
#   mcmc$run(30)
#   samples <- as.matrix(mcmc$mvSamples)
# 
#   test <- as.list(mcmc$mvSamples)
# 
#   expect_identical(unname(samples[,1:3]), test$x)
#   expect_identical(unname(samples[,4:6]), test$y[,1:3,1])
#   expect_identical(unname(samples[,7:9]), test$y[,1:3,2])
#   expect_identical(unname(samples[,10:12]), test$z[,1:3,1,1])
#   expect_identical(unname(samples[,13:15]), test$z[,1:3,2,1])
#   expect_identical(unname(samples[,16:18]), test$z[,1:3,1,2])
#   expect_identical(unname(samples[,19:21]), test$z[,1:3,2,2])
#   expect_identical(unname(samples[,22:24]), test$z[,1:3,1,3])
#   expect_identical(unname(samples[,25:27]), test$z[,1:3,2,3])
# 
#   test <- as.list(mcmc$mvSamples, 'y')
# 
#   expect_identical(names(test), 'y')
#   expect_identical(unname(samples[,4:6]), test$y[,1:3,1])
#   expect_identical(unname(samples[,7:9]), test$y[,1:3,2])
# 
#   ## compiled
#   cm <- compileNimble(m)
#   cmcmc <- compileNimble(mcmc, project = m)
# 
#   cmcmc$run(30)
#   samples <- as.matrix(cmcmc$mvSamples)
# 
#   test <- as.list(cmcmc$mvSamples)
# 
#   expect_identical(unname(samples[,1:3]), test$x)
#   expect_identical(unname(samples[,4:6]), test$y[,1:3,1])
#   expect_identical(unname(samples[,7:9]), test$y[,1:3,2])
#   expect_identical(unname(samples[,10:12]), test$z[,1:3,1,1])
#   expect_identical(unname(samples[,13:15]), test$z[,1:3,2,1])
#   expect_identical(unname(samples[,16:18]), test$z[,1:3,1,2])
#   expect_identical(unname(samples[,19:21]), test$z[,1:3,2,2])
#   expect_identical(unname(samples[,22:24]), test$z[,1:3,1,3])
#   expect_identical(unname(samples[,25:27]), test$z[,1:3,2,3])
# 
#   test <- as.list(cmcmc$mvSamples, 'y')
# 
#   expect_identical(names(test), 'y')
#   expect_identical(unname(samples[,4:6]), test$y[,1:3,1])
#   expect_identical(unname(samples[,7:9]), test$y[,1:3,2])
# }
# )

options(warn = RwarnLevel)
nimbleOptions(verbose = nimbleVerboseSetting)
