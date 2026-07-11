BROWSE_COMPILE_NIMBLE <- FALSE

make_model_calls_methods <- function(code, symTab, auxEnv, info) {
  # convert model_calculate(model, instr) to model$calculate(instr)
  method <- switch(code$name,
    model_calculate = "calculate_impl",
    model_calculateDiff = "calculateDiff_impl",
    model_simulate = "simulate_impl",
    model_getLogProb = "getLogProb_impl"
  )
  new_code <- substitute(
    MODEL$METHOD(INSTRLISTNAME),
    list(MODEL = as.name(code$args[[1]]$name), 
         METHOD = as.name(method),
         INSTRLISTNAME = as.name(code$args[[2]]$name))
  )
  new_expr <- nCompiler::nParse(new_code)
  nCompiler:::replaceArgInCaller(code, new_expr)
  nCompiler:::compile_normalizeCalls(new_expr, symTab, auxEnv)
  NULL
}

values_LAT <- function(code, symTab, auxEnv, info) {
  # convert values(model, nodes) to model$values(nodes)
  browser()
  new_code <- substitute(
    `method->(COPIERS, "copyIntoVector")`,
    list(COPIERS = as.name(code$args[[1]]$name))
  )
  new_expr <- nCompiler::nParse(new_code)
  nCompiler:::replaceArgInCaller(code, new_expr)
  nCompiler:::compile_normalizeCalls(new_expr, symTab, auxEnv)
  NULL
}

nimble_nCompiler_opDefs <- list(
  nimRound = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "round")),
  nimNumeric = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "nNumeric")),
  nimInteger = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "nInteger")),
  nimLogical = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "nLogical")),
  nimMatrix = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "nMatrix")),
  nimC = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "nC")),
  nimRep = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "nRep")),
  nimSeq = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "nSeq")),
  nimDim = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "dim")),
  rexp_nimble = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "rexp_nCompiler")),
  dexp_nimble = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "dexp_nCompiler")),
  nimStep = list(simpleTransformations = list(handler = "replaceAndNormalize", replacement = "nStep")),
  model_calculate = list(matchDef = function(model, instrList) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_calculateDiff = list(matchDef = function(model, instrList) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_simulate = list(matchDef = function(model, instrList) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_getLogProb = list(matchDef = function(model, instrList) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  values = list(matchDef = function(model, nodes) {}, 
    labelAbstractTypes = list(handler = "values_LAT"))
)

proxyNimbleProjectClass <- R6::R6Class(
  classname = "nimbleProjectClass",
  public = list(
    dirName = NULL,
    name = NULL,
    initialize = function(name, dirName) {
      if (!missing(name)) self$name <- name
      if (!missing(dirName)) self$dirName <- dirName
    },
    resetFunctions = function() {},
    clearCompiled = function() {}
  )
)

#' @importFrom nCompiler registerOpDef deregisterOpDef
#' @export
compileNimble <- function(..., project, dirName = NULL, projectName = "",
                          control = list(),
                          resetFunctions = FALSE,
                          showCompilerOutput = getNimbleOption("showCompilerOutput")) {
  ## 1. Extract compilation items
  reset <- FALSE
  ## This pulls out ... arguments, makes names from their expressions if names weren't provided, and combines them with any ... arguments that are lists.
  controlDefaults <- list(
    debug = FALSE, debugCpp = FALSE, compileR = TRUE,
    writeFiles = TRUE, compileCpp = TRUE, loadSO = TRUE,
    returnAsList = FALSE
  )
  # controlDefaults$nCompiler_expandUnits <- TRUE

  dotsDeparses <- unlist(lapply(substitute(list(...))[-1], deparse))
  origList <- list(...)
  if (is.null(names(origList))) names(origList) <- rep("", length(origList))
  boolNoName <- names(origList) == ""
  origIsList <- unlist(lapply(origList, is.list))
  dotsDeparses[origIsList] <- ""
  names(origList)[boolNoName] <- dotsDeparses[boolNoName]
  units <- do.call("c", origList)

  if (any(sapply(units, is, "MCMCconf"))) {
    stop("You have provided an MCMC configuration object, which cannot be compiled. Instead, use run 'buildMCMC' on the configuration object and compile the resulting MCMC object.")
  }
  unitTypes <- getNimbleTypes(units)
  if (length(grep("unknown", unitTypes)) > 0) {
    stop(
      paste0(
        "Some items provided for compilation do not have types that can be compiled: ",
        paste0(names(units), collapse = " "), ".  The types provided were: ",
        paste0(unitTypes, collapse = " "),
        ". Be sure only specialized nimbleFunctions are provided, not nimbleFunction generators."
      ),
      call. = FALSE
    )
  }
  if (is.null(names(units))) names(units) <- rep("", length(units))
  if (length(units) == 0) stop("No objects for compilation provided")

  ## 2. Get project or make new project
  if (missing(project)) {
    if (reset) {
      warning(paste0(
        "You requested 'reset = TRUE', but no project was provided.",
        " If you are trying to re-compiled something into the same project, ",
        "give it as the project argument as well as a compilation item.",
        " For example, 'compileNimble(myFunction, project = myFunction, reset = TRUE)'."
      ))
    }
    if (!is.null(getNimbleOption("nimbleProject"))) {
      project <- getNimbleOption("nimbleProject")
    } else {
      project <- nimbleProjectClass$new(name = projectName) # is dirName needed?
    }

    ## Check for uncompiled models.
    if (!any(sapply(units, is, "RmodelBaseClass"))) {
      mcmcUnits <- which(sapply(units, class) == "MCMC")
      if (any(sapply(mcmcUnits, function(idx) {
        class(units[[idx]]$model$CobjectInterface) == "uninitializedField"
      }))) {
        stop("compileNimble: The model associated with an MCMC is not compiled. Please compile the model first.")
      }
    }
  } else {
    project <- getNimbleProject(project, TRUE)
    if (!inherits(project, "nimbleProjectClass")) {
      stop("Invalid project argument; note that models and nimbleFunctions need to be compiled before they can be used to specify a project. Once compiled you can use an R model or nimbleFunction to specify the project.", call. = FALSE)
    }
  }
  if (resetFunctions) project$resetFunctions()

  for (i in names(controlDefaults)) {
    if (!i %in% names(control)) control[[i]] <- controlDefaults[[i]]
  }

  if (!showCompilerOutput) {
    messageIfVerbose("Compiling via nCompiler\n  [Note] This may take a minute.\n  [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.")
  }
  if (showCompilerOutput) {
    messageIfVerbose("Compiling via nCompiler\n  [Note] This may take a minute.\n  [Note] On some systems there may be some compiler warnings that can be safely ignored.")
  }

  #
  # if (isTRUE(control[["nCompiler_expandUnits"]])) {
  #   expandedUnits <- compileNimble_expandUnits(units, unitTypes)
  #   units <- expandedUnits$units
  #   unitTypes <- expandedUnits$unitTypes
  #   units_extraNames <- expandedUnits$extraNames
  # }
  # foundUnitsEnv <- new.env()
  #

  # ans may become superfluous
  ans <- list()
  # nComp_units <- vector(mode = "list", length = length(units))
  rcfUnits <- unitTypes == "rcf"
  if (sum(rcfUnits) > 0) {
    whichUnits <- which(rcfUnits)
    for (i in whichUnits) {
      if (isTRUE(getNimbleOption("enableDerivs"))) {
        if (!isFALSE(environment(units[[i]])$nfMethodRCobject$buildDerivs)) {
          stop(paste0(
            "A nimbleFunction without setup code and with buildDerivs = TRUE can't be included\n",
            "directly in a call to compileNimble.  It can be called by another nimbleFunction and,\n",
            "in that case, will be automatically compiled."
          ))
        }
      }
      # # nComp_units[[i]] <- RCfun_2_nFun(units[[i]], foundUnitsEnv)
      # nComp_units[[i]] <- units[[i]]
      # foundUnitsEnv[[names(units)[i]]] <- nComp_units[[i]]
      # if (isTRUE(control[["nCompiler_expandUnits"]])) {
      #   for (EN in units_extraNames[[i]]) {
      #     foundUnitsEnv[[EN]] <- nComp_units[[i]]
      #   }
      # }
      # environment(units[[i]])$nfMethodRCobject[["nimbleProject"]] <- project
      ans[[i]] <-
        project$RCfunction_add(units[[i]], control = control)
      if (names(units)[i] != "") names(ans)[i] <- names(units)[i]
    }
  }

  modelUnits <- unitTypes == "model"
  if (sum(modelUnits) > 0) {
    whichUnits <- which(modelUnits)
    for (i in whichUnits) {
      ans[[i]] <- project$model_add(units[[i]], control = control)
      if (names(units)[i] != "") names(ans)[i] <- names(units)[i]
    }
  }

  nfUnits <- unitTypes == "nf"
  if (sum(nfUnits) > 0) {
    whichUnits <- which(nfUnits)
    nfAns <- project$nimbleFunction_add_multi(units[whichUnits], control = control)
    ans[whichUnits] <- nfAns
    for (i in whichUnits) if (names(units)[i] != "") names(ans)[i] <- names(units)[i]
  }

  # From here we are ready to:
  # Have the project create the nfProcs
  # Collect compilation units
  # Call nCompile
  # Build and populate objects

  if (isTRUE(.GlobalEnv$BROWSE_COMPILE_NIMBLE)) browser()

  project$process()
  nComp_units <- project$get_nComp_units()

  # names(nComp_units) <- names(units)
  nCompiler::registerOpDef(nimble_nCompiler_opDefs)
  on.exit({
    nCompiler::deregisterOpDef(ls(nimble_nCompiler_opDefs))
  })
  nCompile_results <- do.call(nCompiler::nCompile, c(nComp_units, list(returnList = TRUE)))

  if (isTRUE(.GlobalEnv$BROWSE_COMPILE_NIMBLE)) browser()

  compiled_units <- vector("list", length = length(units))

  if (sum(rcfUnits) > 0) {
    whichUnits <- which(rcfUnits)
    for (i in whichUnits) {
      this_name <- nCompiler::NFinternals(units[[i]])$uniqueName
      compiled_units[[i]] <- nCompile_results[[this_name]]
    }
  }

  project$instantiate(nCompile_results)

  if (sum(modelUnits) > 0) {
    whichUnits <- which(modelUnits)
    for (i in whichUnits) {
      compiled_units[[i]] <- project$model_getResults(units[[i]])
    }
  }

  if (sum(nfUnits) > 0) {
    whichUnits <- which(nfUnits)
    compiled_units[whichUnits] <- project$nimbleFunction_getResults(units[whichUnits])
  }
  names(compiled_units) <- names(units)
  if (length(compiled_units) == 1) compiled_units[[1]] else compiled_units
}

getNimbleTypes <- function(units) {
  ans <- character(length(units))
  for (i in seq_along(units)) {
    if (inherits(units[[i]], "modelBase_nClass")) {
      ans[i] <- "model"
    } else if (is.nf(units[[i]])) {
      ans[i] <- "nf"
    } ## a nimbleFunction
    else if (is.rcf(units[[i]])) {
      ans[i] <- "rcf"
    } ## an RCfunction = a nimbleFunction with no setup
    else if (is.nfGenerator(units[[i]])) {
      ans[i] <- "unknown(nf generator)"
    } else if (is.nl(units[[i]])) {
      ans[i] <- "nl"
    } ## a nimbleList
    else {
      ans[i] <- "unknown"
    }
  }
  ans
}

# return the nimble project, if any, associated with a model or nimbleFunction object.
# This feature needs attention. It may be redesigned or deprecated.
getNimbleProject <- function(project, stopOnNull = FALSE) {
  if (inherits(project, "nimbleProjectClass")) {
    return(project)
  }
  # From here down, this has not been updated to nimble2.
  if (is.nf(project)) {
    return(nfVar(project, "nimbleProject"))
  }
  if (is.rcf(project)) {
    return(environment(project)$nfMethodRCobject$nimbleProject)
  }
  ans <- try(project$nimbleProject)
  if (inherits(ans, "try-error") | is.null(ans)) {
    if (stopOnNull) stop(paste0("cannot determine nimbleProject from provided project argument"))
    return(NULL)
  }
  ans
}
