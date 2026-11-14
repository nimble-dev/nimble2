# These need to be turned into actual tests.

code <- nimbleCode({
    for(i in 1:3) {
      y[i]~dnorm(mu[i],var=sigma2[i])
      mu[i] ~ dunif(-10,10)
    }
    z[1:2] <- sigma2[4:5]
    w[1:3] ~ dmnorm(zeros[1:3],pr[1:3,1:3])
})
inits <- list(mu = c(-1,0,1), sigma2 = c(1,2,3,4,5),
              zeros = rep(0,3), pr = diag(3))
data <- list(y = rnorm(3))

m <- nimbleModel(code, inits = inits, data=data)
initModel <- initializeModel(m)
cobj <- compileNimble(m, initModel)

# Test out various NA cases, including logProb

# RHS only and determ nodes
cobj$m$sigma2[1] <- NA
cobj$initModel$run()
cobj$m$sigma2[1] <- 1
cobj$m$sigma2[5] <- NA
cobj$initModel$run()

cobj$m$sigma2 <- runif(5)

# Data nodes
cobj$m$y[1] <- NA
cobj$initModel$run()
cobj$m$y[1] <- 1e12
cobj$initModel$run()

cobj$m$y[1] <- 0

# Stoch nodes
cobj$m$mu[1] <- NA
cobj$initModel$run()  # mu should be simulated; no error
cobj$m$mu[1]
cobj$m$mu[1] <- 30
cobj$initModel$run()

# Multivariate case with a single NA
cobj$m$zeros[2] <- NA
cobj$initModel$run() 
cobj$m$zeros[2] <- 0
cobj$m$w[2] <- NA
cobj$initModel$run()  # w should be simulated; no error
cobj$m$w
