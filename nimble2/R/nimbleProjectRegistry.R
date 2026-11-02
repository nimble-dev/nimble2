# Registry of the nimbleProject that most recently compiled each object.
#
# This lets a default project be found from an object (a model, a nimbleFunction
# instance, a modelValues or nimbleList object, or any of their compiled
# counterparts) without modifying the object. It does not restrict how objects
# are compiled: the same object can be compiled in several projects, each with its
# own independent compiled counterpart. The registry records only the most recent
# one, which is used when an object is given as compileNimble's project argument,
# and with nimbleOptions(legacy_nimble = TRUE) (see compileNimble).
#
# The registry is an environment created in .onLoad. Each entry is named by the
# object's environment address label and holds a list of weak references with
# key = the object's environment and value = the project. A lookup checks the
# weak reference key with identical(), so a reused address can never match.
# There can be more than one weak reference under a label only if an object was
# collected and its address reused before pruning.
#
# R weak references are ephemerons: the value (project) is kept alive only while
# the key (object) is reachable from somewhere other than the value. A project
# holds its objects strongly, but that does not keep the registry entries alive.
# When nothing outside a project refers to any of its objects, the project and
# its objects can be garbage collected, and the dead entries are removed the
# next time the registry is pruned.

nimbleProject_registry_key <- function(obj) {
  # nimbleFunction instances are reference class objects; the project labels them by as.environment.
  if (is.nf(obj)) return(as.environment(obj))
  # models, modelValues, nimbleLists and compiled objects (R6 objects or loadedObjectEnv) are environments
  if (is.environment(obj)) return(obj)
  NULL
}

nimbleProject_registry_lookup <- function(obj) {
  key <- nimbleProject_registry_key(obj)
  if (is.null(key)) return(NULL)
  weakrefs <- nimbleProject_registry[[rlang::env_label(key)]]
  for (weakref in weakrefs) {
    if (identical(rlang::wref_key(weakref), key)) return(rlang::wref_value(weakref))
  }
  NULL
}

# Record that obj was most recently compiled in project, replacing any earlier record.
nimbleProject_registry_add <- function(obj, project) {
  key <- nimbleProject_registry_key(obj)
  if (is.null(key)) return(invisible(NULL))
  label <- rlang::env_label(key)
  weakrefs <- nimbleProject_registry[[label]]
  for (i in seq_along(weakrefs)) {
    if (identical(rlang::wref_key(weakrefs[[i]]), key)) {
      if (!identical(rlang::wref_value(weakrefs[[i]]), project)) {
        weakrefs[[i]] <- rlang::new_weakref(key, project)
        nimbleProject_registry[[label]] <- weakrefs
      }
      return(invisible(NULL))
    }
  }
  nimbleProject_registry_prune()
  nimbleProject_registry[[label]] <- c(nimbleProject_registry[[label]], list(rlang::new_weakref(key, project)))
  invisible(NULL)
}

# Return the projects that most recently compiled any models among objs or used
# in their setup code (searched recursively through nested nimbleFunctions and
# nimbleFunctionLists). Each project is included once.
# This is used only with nimbleOptions(legacy_nimble = TRUE) (see compileNimble).
nimbleProject_registry_lookup_models <- function(objs) {
  projects <- list()
  visited <- new.env(parent = emptyenv())
  visit <- function(obj) {
    if (inherits(obj, "nimbleFunctionList")) {
      for (f in obj$contentsList) visit(f)
      return(invisible(NULL))
    }
    key <- nimbleProject_registry_key(obj)
    if (is.null(key)) return(invisible(NULL))
    label <- rlang::env_label(key)
    if (!is.null(visited[[label]])) return(invisible(NULL))
    visited[[label]] <- TRUE
    if (inherits(obj, "modelBase_nClass")) {
      project <- nimbleProject_registry_lookup(obj)
      if (!is.null(project) && !any(vapply(projects, \(x) identical(x, project), logical(1))))
        projects <<- c(projects, list(project))
    } else if (is.nf(obj)) {
      for (name in nf_getSetupOutputNames(obj)) visit(obj[[name]])
    }
    invisible(NULL)
  }
  for (obj in objs) visit(obj)
  projects
}

# Remove weak references whose objects have been garbage collected.
nimbleProject_registry_prune <- function() {
  for (label in ls(nimbleProject_registry, all.names = TRUE)) {
    weakrefs <- nimbleProject_registry[[label]]
    alive <- vapply(weakrefs, \(weakref) !is.null(rlang::wref_key(weakref)), logical(1))
    if (!any(alive)) {
      rm(list = label, envir = nimbleProject_registry)
    } else if (!all(alive)) {
      nimbleProject_registry[[label]] <- weakrefs[alive]
    }
  }
  invisible(NULL)
}

# Number of live entries, after pruning. Used for testing.
nimbleProject_registry_size <- function() {
  nimbleProject_registry_prune()
  sum(vapply(ls(nimbleProject_registry, all.names = TRUE),
             \(label) length(nimbleProject_registry[[label]]), integer(1)))
}
