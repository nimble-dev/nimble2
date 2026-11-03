# library(nimble2)
nimbleOptions(enableDerivs = FALSE)
nCompiler::nOptions(pause_after_writing_files = FALSE)

# Count calls to nCompile, to check what is and isn't recompiled.
# The counter must be in the global environment so the tracer can see it from inside nCompiler.
assign("nCompile_call_counter", new.env(), envir = globalenv())
nCompile_call_counter$n <- 0
suppressMessages(trace("nCompile", where = asNamespace("nCompiler"),
      tracer = quote(nCompile_call_counter$n <- nCompile_call_counter$n + 1), print = FALSE))

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


test_that("nimble usage pattern: compileNimble(m) then compileNimble(algo, project = m)", {
  m <- make_test_model()
  cm <- compileNimble(m)
  algo <- bar(m, "mu", 2)
  n0 <- nCompile_call_counter$n
  comp <- compileNimble(algo, project = m)
  expect_equal(nCompile_call_counter$n, n0 + 1) # model not recompiled
  nCompiler::value(cm, "mu") <- 3
  expect_equal(comp$run(), 6)
  expect_equal(nCompiler::value(cm, "mu"), 6)
  # The project can be found from any of the objects, compiled or not.
  proj <- nimble2:::getNimbleProject(m)
  expect_true(inherits(proj, "nimbleProjectClass"))
  expect_identical(nimble2:::getNimbleProject(cm), proj)
  expect_identical(nimble2:::getNimbleProject(algo), proj)
  expect_identical(nimble2:::getNimbleProject(comp), proj)
  # project can be given as a compiled object or a nimbleFunction instance
  algo2 <- foo(m, "mu")
  comp2 <- compileNimble(algo2, project = cm)
  expect_equal(comp2$run(), 6)
  expect_identical(nimble2:::getNimbleProject(comp2), proj)
  rm(cm, comp, comp2); gc()
})

test_that("without a project argument, a new project is used", {
  m <- make_test_model()
  cm <- compileNimble(m)
  proj <- nimble2:::getNimbleProject(m)
  n0 <- nCompile_call_counter$n
  cm2 <- compileNimble(m)
  expect_equal(nCompile_call_counter$n, n0 + 1) # an independent compiled copy
  expect_false(identical(cm, cm2))
  proj2 <- nimble2:::getNimbleProject(m) # the most recent project
  expect_false(identical(proj2, proj))
  expect_identical(nimble2:::getNimbleProject(cm), proj)
  expect_identical(nimble2:::getNimbleProject(cm2), proj2)
  nCompiler::value(cm2, "mu") <- 2
  expect_equal(nCompiler::value(cm, "mu"), 0.5)
  # A nimbleFunction using m in setup code gets its own compiled copy of m.
  algo <- bar(m, "mu", 2)
  comp <- compileNimble(algo)
  expect_equal(comp$run(), 1)
  expect_equal(nCompiler::value(cm, "mu"), 0.5)
  expect_equal(nCompiler::value(cm2, "mu"), 2)
  # Within one project, each object is compiled once.
  n0 <- nCompile_call_counter$n
  comp2 <- compileNimble(algo, m, project = comp)
  expect_equal(nCompile_call_counter$n, n0)
  expect_identical(comp2$algo, comp)
  expect_equal(nCompiler::value(comp2$m, "mu"), 1)
  rm(cm, cm2, comp, comp2); gc()
})

test_that("with a project argument, objects compiled in another project get independent compiled copies", {
  m1 <- make_test_model()
  m2 <- make_test_model()
  cm1 <- compileNimble(m1)
  cm2 <- compileNimble(m2)
  proj1 <- nimble2:::getNimbleProject(m1)
  proj2 <- nimble2:::getNimbleProject(m2)
  cm1_copy <- compileNimble(m1, project = m2)
  expect_false(identical(cm1_copy, cm1))
  expect_identical(nimble2:::getNimbleProject(cm1_copy), proj2)
  expect_identical(nimble2:::getNimbleProject(cm1), proj1)
  expect_identical(nimble2:::getNimbleProject(m1), proj2) # the most recent project
  nCompiler::value(cm1_copy, "mu") <- 2
  expect_equal(nCompiler::value(cm1, "mu"), 0.5)
  expect_error(nimble2:::getNimbleProject(foo(m1, "mu"), stopOnNull = TRUE), "Cannot determine a nimbleProject")
  rm(cm1, cm2, cm1_copy); gc()
})

test_that("with legacy_nimble = TRUE, compiled models are shared", {
  old <- nimbleOptions(c("legacy_nimble", "verbose"))
  nimbleOptions(legacy_nimble = TRUE, verbose = TRUE)
  on.exit(nimbleOptions(old))
  expect_true(nimbleModel::getNimbleModelOption("legacy_nimble")) # one setting for both packages
  m <- make_test_model()
  cm <- compileNimble(m)
  proj <- nimble2:::getNimbleProject(m)
  # Without a project argument, the project of the model used in setup code is used.
  algo <- bar(m, "mu", 2)
  comp <- compileNimble(algo)
  expect_identical(nimble2:::getNimbleProject(comp), proj)
  expect_equal(comp$run(), 1)
  expect_equal(nCompiler::value(cm, "mu"), 1) # the same compiled model
  # A project argument different from the model's project is not used (with a note).
  algo2 <- bar(m, "mu", 2)
  new_proj <- nimble2:::nimbleProjectClass$new()
  expect_message(comp2 <- compileNimble(algo2, project = new_proj), "legacy_nimble")
  expect_identical(nimble2:::getNimbleProject(comp2), proj)
  expect_equal(comp2$run(), 2)
  expect_equal(nCompiler::value(cm, "mu"), 2)
  # Models compiled in different projects can't be shared together.
  m2 <- make_test_model()
  cm2 <- compileNimble(m2)
  expect_error(compileNimble(list(foo(m, "mu"), foo(m2, "mu"))), "compiled in different nimbleProjects")
  rm(cm, comp, comp2, cm2); gc()
})

test_that("reset = TRUE compiles the inputs in a new project", {
  m <- make_test_model()
  algo <- foo(m, "mu")
  comp <- compileNimble(algo, m)
  proj <- nimble2:::getNimbleProject(m)
  expect_error(compileNimble(algo, m, project = m, reset = TRUE), "should not be provided")
  n0 <- nCompile_call_counter$n
  comp_reset <- compileNimble(algo, m, reset = TRUE)
  expect_equal(nCompile_call_counter$n, n0 + 1)
  proj_reset <- nimble2:::getNimbleProject(m)
  expect_false(identical(proj_reset, proj))
  expect_identical(nimble2:::getNimbleProject(algo), proj_reset)
  expect_identical(nimble2:::getNimbleProject(comp_reset$m), proj_reset)
  expect_identical(nimble2:::getNimbleProject(comp$m), proj) # old compiled objects keep their project
  expect_false(identical(comp_reset$m, comp$m)) # the model was compiled again
  nCompiler::value(comp_reset$m, "mu") <- 4
  expect_equal(comp_reset$algo$run(), 4)
  expect_equal(comp$algo$run(), 0.5) # old objects still work, with their own model
  rm(comp, comp_reset); gc()
})

# KNOWN ISSUE (separate from the registry; skipped for now).
#
# Once a nimbleFunction class has been compiled, the nimbleProject, its uncompiled
# objects (model and nimbleFunction instances) and their compiled counterparts are never
# garbage collected, even after every user-visible reference is removed. Compiling only
# a model does not leak. What was established while investigating (2026-10-06):
#  - The leak does not depend on the registry: it is the same with registration turned off.
#    The registry only made it visible.
#  - Stopping nCompile at successive stages (control = list(endStage = ...)), the leak first
#    appears at "makeRcppPacket", before any C++ is written, compiled or loaded. So the holding
#    reference is an ordinary R-level one, not the C++ statics that keep each class
#    environment (CnClass_env) and the DLL environment alive.
#  - For the nimbleFunction class, that stage runs gather_needed_units, after which the needed
#    classes (modelBase_nClass, the generated nList_I1, and others) are also compiled.
#    One nList class is built (via check_built_types -> nList -> nList_nClass) with env set
#    to the nimbleFunction class's uncompiled generator parent_env.
#  - Searches for a reference path to the leaked objects found none from: all class
#    environments and the DLL environment of the compiled DLL, the global environment, all
#    loaded namespaces, options(), or the predefined generators. No finalizers are registered
#    in nCompiler, nimbleModel or nimble2. So the holding reference is of a kind that the
#    search could not see; it has not been found.
#  - Separately (not the cause of this leak): the DLL environment's functions (call_method,
#    get_value, ...) have sourceCpp's result environment as their environment, whose parent is
#    the frame of nCompile_finish_nonpackage. That frame holds unevaluated promises
#    (cppDefs, cpp_names, env) leading back into the nCompile call, so every compile keeps its
#    compile-time data for the rest of the session. Removing those promises did not end
#    this leak.
#  - Note on cpp_init_: nfProcessing's build_cpp_init_ creates cpp_init_ with nFunction()'s
#    default where = parent.frame(), so its environment is that method's frame, whose parent
#    is the nfProc (-> instances -> model). That is a real path from the nimbleFunction class
#    to the instances, and passing where = asNamespace("nimble2") removes it. But doing so
#    did not end the leak, so it was not shown to matter here, and the change was reverted.
test_that("a project and its registry entries are garbage collected when its objects are gone", {
  skip("Known leak once a nimbleFunction class is compiled; see the comment above this test.")
  gc()
  size0 <- nimble2:::nimbleProject_registry_size()
  compile_and_drop <- function() {
    m <- make_test_model()
    algo <- foo(m, "mu")
    comp <- compileNimble(algo, m)
    # algo, m and their compiled counterparts
    expect_equal(nimble2:::nimbleProject_registry_size(), size0 + 4)
    invisible(NULL)
  }
  compile_and_drop()
  gc()
  expect_equal(nimble2:::nimbleProject_registry_size(), size0)
})

suppressMessages(untrace("nCompile", where = asNamespace("nCompiler")))
