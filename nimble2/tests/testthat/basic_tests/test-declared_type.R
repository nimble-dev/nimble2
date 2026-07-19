# test the feature (new to nimble2) of allowing an attribute
# "nimble_type" to provide nDim, type or size.

library(nimble2)

nimbleOptions(enableDerivs = FALSE)

## setNimType <- function(x, value) {
##   attr(x, "nimble_type") <- value
##   x
## }

test_that("attribute nimble_type works", {
  currOpt <- getNimbleOption("deduceTypeFromFirstInstanceOnly")
  on.exit(nimble2:::setNimbleOption("deduceTypeFromFirstInstanceOnly", currOpt))

  for(nopt in c(FALSE, TRUE)) {
    nimble2:::setNimbleOption("deduceTypeFromFirstInstanceOnly", nopt)
    ## STOPPED HERE: THIS IS NOT PROPAGATING.
    foo <- nimbleFunction(
      setup = function( ) {
        xDv_s <- 1:2 |> setNimType(list(nDim = 0)) # set up for an err
        setupOutputs(xDv_s)
      },
      run = function() {
      },
      check=FALSE
    )
    foo1 <- foo()
    expect_error(cfoo1 <- compileNimble(foo1))

    foo <- nimbleFunction(
      setup = function( ) {
        xDs <- 1 # N.B. A solo 1 induces double
        xIs <- 1L
        xDv <- 1.1:2.2 # N.B. 1:2 induces integer
        xIv <- 1L:2L
        #
        xDs_v <- 1 |> setNimType(list(nDim = 1))
        xDs_vI <- 1 |> setNimType(list(nDim = 1, type = "integer"))
        xDv_vI <- 1.1:2.2 |> setNimType(list(type = "integer"))

        setupOutputs(xDs, xIs, xDv, xIv,
                     xDs_v, xDs_vI, xDv_vI)
      },
      run = function() {
      },
      check=FALSE
    )
    foo1 <- foo()

    cfoo1 <- compileNimble(foo1)
    cfoo1$xDs <- 2.3 # should be a scalar double
    expect_equal(cfoo1$xDs, 2.3)
    expect_error(cfoo1$xDs <- c(2.3, 3.4))

    cfoo1$xIs <- 2.3 # should be a scalar integer
    expect_equal(cfoo1$xIs, 2)
    expect_error(cfoo1$xIs <- 2:3)

    cfoo1$xDv <- c(2.3, 3.4) # should be vector double
    expect_equal(cfoo1$xDv, c(2.3, 3.4))

    cfoo1$xIv <- c(2.3, 3.4) # should be vector integer
    expect_equal(cfoo1$xIv, c(2, 3))

    cfoo1$xDs_v <- c(2.3, 3.4)
    expect_equal(cfoo1$xDs_v, c(2.3, 3.4)) # vector double by declaration

    cfoo1$xDs_vI <- c(2.3, 3.4)
    expect_equal(cfoo1$xDs_vI, c(2, 3)) # vector integer by declaration

    cfoo1$xDv_vI <- c(2.3, 3.4)
    expect_equal(cfoo1$xDv_vI, c(2, 3)) # vector integer by declaration

    rm(cfoo1); gc()
  }
  nimble2:::setNimbleOption("deduceTypeFromFirstInstanceOnly", currOpt)
})
