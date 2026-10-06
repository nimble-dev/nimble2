# Registry of which nimbleProject each object belongs to.
#
# This lets a project be recovered from an object (a model, a nimbleFunction
# instance, a modelValues or nimbleList object, or any of their compiled
# counterparts) without modifying the object.
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

# replace = TRUE allows an object already registered to a different project to be
# moved to this project (used by compileNimble(..., reset = TRUE)).
nimbleProject_registry_add <- function(obj, project, replace = FALSE) {
  key <- nimbleProject_registry_key(obj)
  if (is.null(key)) return(invisible(NULL))
  label <- rlang::env_label(key)
  weakrefs <- nimbleProject_registry[[label]]
  for (i in seq_along(weakrefs)) {
    if (identical(rlang::wref_key(weakrefs[[i]]), key)) {
      current_project <- rlang::wref_value(weakrefs[[i]])
      if (identical(current_project, project)) return(invisible(NULL))
      if (!replace) {
        stop(paste0(
          "An object being compiled already belongs to a different nimbleProject.",
          " Compile it in the same project as before (for example, by providing it or a related object as the project argument),",
          " or use reset = TRUE to compile it in a new project."
        ), call. = FALSE)
      }
      weakrefs[[i]] <- rlang::new_weakref(key, project)
      nimbleProject_registry[[label]] <- weakrefs
      return(invisible(NULL))
    }
  }
  nimbleProject_registry_prune()
  nimbleProject_registry[[label]] <- c(nimbleProject_registry[[label]], list(rlang::new_weakref(key, project)))
  invisible(NULL)
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
