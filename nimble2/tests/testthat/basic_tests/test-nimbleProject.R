# library(nimble2)
nimbleOptions(enableDerivs = FALSE)
nCompiler::nOptions(pause_after_writing_files = FALSE)

# Count calls to nCompile, to check what is and isn't recompiled.
# The counter must be in the global environment so the tracer can see it from inside nCompiler.
assign("nCompile_call_counter", new.env(), envir = globalenv())
nCompile_call_counter$n <- 0
suppressMessages(trace("nCompile", where = asNamespace("nCompiler"),
      tracer = quote(nCompile_call_counter$n <- nCompile_call_counter$n + 1), print = FALSE))

test_that("RCfunction compiled twice in a project is not recompiled", {
  proj <- nimble2:::nimbleProjectClass$new()
  rc1 <- nimbleFunction(run = function(x = double()) {return(x + 1); returnType(double())})
  n0 <- nCompile_call_counter$n
  c1 <- compileNimble(rc1, project = proj)
  expect_equal(nCompile_call_counter$n, n0 + 1)
  c1b <- compileNimble(rc1, project = proj)
  expect_equal(nCompile_call_counter$n, n0 + 1)
  expect_identical(c1, c1b)
  expect_equal(c1b(2), 3)
})

test_that("model compiled twice in a project returns the same compiled model", {
  proj <- nimble2:::nimbleProjectClass$new()
  code <- quote({ mu ~ dnorm(0, 1); for(i in 1:3) y[i] ~ dnorm(mu, 1) })
  m <- nimbleModel::nimbleModel(code, inits = list(mu = 0.5), data = list(y = 1:3), returnClass = TRUE)$new()
  n0 <- nCompile_call_counter$n
  cm <- compileNimble(m, project = proj)
  nCompiler::value(cm, "mu") <- 2.5
  cm2 <- compileNimble(m, project = proj)
  expect_equal(nCompile_call_counter$n, n0 + 1)
  expect_identical(cm, cm2)
  expect_equal(nCompiler::value(cm2, "mu"), 2.5) # not repopulated
  rm(cm, cm2); gc()
})

# Model and nimbleFunction generators shared by the tests below.
# foo reads a node of the model.
# bar reads a node of the model, multiplies it by k, and writes the result back to the model,
# so it can be checked that it is using the same compiled model as other objects.
make_test_model <- function() {
  code <- quote({ mu ~ dnorm(0, 1); for(i in 1:3) y[i] ~ dnorm(mu, 1) })
  nimbleModel::nimbleModel(code, inits = list(mu = 0.5), data = list(y = 1:3), returnClass = TRUE)$new()
}
foo <- nimbleFunction(
  setup = function(m_, node) {},
  run = function() { ans <- m_[[node]]; return(ans); returnType(double()) },
  check = FALSE)
bar <- nimbleFunction(
  setup = function(m_, node, k) {},
  run = function() { ans <- k * m_[[node]]; m_[[node]] <<- ans; return(ans); returnType(double()) },
  check = FALSE)

test_that("model + nimbleFunction compiled again returns the existing compiled objects", {
  proj <- nimble2:::nimbleProjectClass$new()
  m <- make_test_model()
  obj <- foo(m, "mu")
  n0 <- nCompile_call_counter$n
  comp <- compileNimble(obj, m, project = proj)
  expect_equal(nCompile_call_counter$n, n0 + 1)
  expect_equal(comp$obj$run(), 0.5)
  comp2 <- compileNimble(obj, m, project = proj)
  expect_equal(nCompile_call_counter$n, n0 + 1)
  expect_identical(comp$obj, comp2$obj)
  expect_identical(comp$m, comp2$m)
  rm(comp, comp2); gc()
})

test_that("a new nimbleFunction generator using an already compiled model uses the same compiled model", {
  proj <- nimble2:::nimbleProjectClass$new()
  m <- make_test_model()
  obj <- foo(m, "mu")
  n0 <- nCompile_call_counter$n
  comp <- compileNimble(obj, m, project = proj)
  objb <- bar(m, "mu", 2)
  cb <- compileNimble(objb, project = proj)
  expect_equal(nCompile_call_counter$n, n0 + 2) # bar compiled alone; model not recompiled
  expect_identical(compileNimble(m, project = proj), comp$m)
  # cb must be populated with the existing compiled model, comp$m.
  nCompiler::value(comp$m, "mu") <- 3
  expect_equal(cb$run(), 6)
  expect_equal(nCompiler::value(comp$m, "mu"), 6)
  expect_equal(comp$obj$run(), 6) # earlier object sees the same model
  rm(comp, cb); gc()
})

# These error traps are expected to become unnecessary once all stages are
# done, when new instances of compiled generators will be supported.
test_that("a new instance of an already compiled nimbleFunction generator is trapped (temporary)", {
  proj <- nimble2:::nimbleProjectClass$new()
  m <- make_test_model()
  obj <- foo(m, "mu")
  comp <- compileNimble(obj, m, project = proj)
  obj_new <- foo(m, "mu")
  expect_error(compileNimble(obj_new, project = proj), "not supported yet")
  expect_error(compileNimble(obj, obj_new, project = proj), "not supported yet")
  rm(comp); gc()
})

test_that("resetFunctions = TRUE keeps the compiled model and recompiles nimbleFunctions from scratch", {
  proj <- nimble2:::nimbleProjectClass$new()
  m <- make_test_model()
  obj <- foo(m, "mu")
  comp <- compileNimble(obj, m, project = proj)
  objb <- bar(m, "mu", 3)
  n0 <- nCompile_call_counter$n
  # obj was compiled before the reset and must be recompiled; objb is new.
  comp_reset <- compileNimble(obj, objb, project = proj, resetFunctions = TRUE)
  expect_equal(nCompile_call_counter$n, n0 + 1)
  expect_length(proj$NFgens, 2)
  expect_length(proj$modelGens, 1)
  expect_false(identical(comp_reset$obj, comp$obj)) # a new compiled object
  expect_identical(compileNimble(m, project = proj), comp$m) # model not recompiled
  # All objects use the same compiled model.
  nCompiler::value(comp$m, "mu") <- 2
  expect_equal(comp_reset$obj$run(), 2)
  expect_equal(comp_reset$objb$run(), 6)
  expect_equal(nCompiler::value(comp$m, "mu"), 6)
  expect_equal(comp_reset$obj$run(), 6)
  expect_equal(comp$obj$run(), 6) # the object compiled before the reset still works
  rm(comp, comp_reset); gc()
})

suppressMessages(untrace("nCompile", where = asNamespace("nCompiler")))
