BROWSE_COMPILE_NIMBLE <- FALSE

make_model_calls_methods <- function(code, symTab, auxEnv, info) {
  # convert model_calculate(model, instrList) to model$calculate(instrList),
  # model_calculate_indexed(model, instrList, i) to model$calculate_indexed(instrList, i), etc.
  # nimbleModel's opDefs for modelBase_nClass then turn model$calculate(...) into
  # model$calculate_impl(...) etc. in labelAbstractTypes.
  method <- switch(code$name,
    model_calculate = "calculate",
    model_calculateDiff = "calculateDiff",
    model_simulate = "simulate",
    model_getLogProb = "getLogProb",
    model_getBound = "getBound",
    model_getParam = "getParam",
    model_calculate_indexed = "calculate_indexed",
    model_calculateDiff_indexed = "calculateDiff_indexed",
    model_simulate_indexed = "simulate_indexed",
    model_getLogProb_indexed = "getLogProb_indexed",
    model_getBound_indexed = "getBound_indexed",
    model_getParam_indexed = "getParam_indexed",
    stop("In make_model_calls_methods, unknown model call ", code$name)
  )
  # Arguments after the model can be expressions (e.g. an index i + 1), so deparse them.
  args <- lapply(code$args[-1], \(arg) parse(text = nCompiler::nDeparse(arg), keep.source = FALSE)[[1]])
  new_code <- as.call(c(
    list(call("$", as.name(code$args[[1]]$name), as.name(method))),
    args
  ))
  new_expr <- nCompiler::nParse(new_code)
  nCompiler:::replaceArgInCaller(code, new_expr)
  nCompiler:::compile_normalizeCalls(new_expr, symTab, auxEnv)
  NULL
}

getOrSetValues_LAT_impl <- function(code, symTab, auxEnv, info) {
  # convert getOrSetValues_(multiCopier) to multiCopier$getValues() or another invocation for setting
  # arguably it would be a cleaner use of compilation steps to only set
  # type here and then choose method code in the cpp output step.
  copiersName <- as.name(code$args[[1]]$name)
  if(isTRUE(auxEnv$onLHS)) {
    # setValues_() refreshes the data pointers and returns a proxy whose operator= does the copy.
    new_code <- substitute(
      M(COPIERS, "setValues_"),
      list(M = as.name("->method"),
           COPIERS = copiersName)
    )
  } else {
    new_code <- substitute(
      M(COPIERS, "getValues"),
      list(M = as.name("->method"),
           COPIERS = copiersName)
    )
  }
  new_expr <- nCompiler::nParse(new_code)
  nCompiler:::replaceArgInCaller(code, new_expr)
  nCompiler:::compile_normalizeCalls(new_expr, symTab, auxEnv)
  new_expr$type <- nCompiler:::type2symbol("numericVector")
  NULL
}

getOrSetValues_indexed_LAT_impl <- function(code, symTab, auxEnv, info) {
  # convert getOrSetValues_indexed_(multiCopier, i, mode) to multiCopier->getValues_raw_(i),
  # multiCopier->getValues_scalar_(i), or, on the LHS of an assignment, the corresponding
  # setValues_raw_(i) or setValues_scalar_(i), which return proxies whose operator= does the copy.
  modeArg <- code$args[[3]] # This should really be a compileArg, but it is only placed through internal (keyword) processing, so not an issue.
  if (!(isTRUE(modeArg$isLiteral) && is.character(modeArg$name) &&
        modeArg$name %in% c("raw", "scalar"))) {
    stop("In getOrSetValues_indexed_, mode must be a literal \"raw\" or \"scalar\".")
  }
  onLHS <- isTRUE(auxEnv$onLHS)
  # Label the index as an ordinary (RHS) expression, even when the values() call is on the LHS.
  auxEnv$onLHS <- FALSE
  on.exit(auxEnv$onLHS <- onLHS)
  inserts <- nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(
    code, symTab, auxEnv, info, useArgs = c(FALSE, TRUE, FALSE)
  )
  auxEnv$onLHS <- onLHS
  on.exit(NULL)
  method <- paste0(if (onLHS) "setValues_" else "getValues_", modeArg$name, "_")
  iExpr <- nCompiler:::removeArg(code, 2)
  new_code <- substitute(
    M(COPIERS, METHOD, I),
    list(M = as.name("->method"),
         COPIERS = as.name(code$args[[1]]$name),
         METHOD = method,
         I = 0L) # placeholder for the labeled index expression
  )
  new_expr <- nCompiler::nParse(new_code)
  nCompiler:::setArg(new_expr, 3, iExpr)
  nCompiler:::replaceArgInCaller(code, new_expr)
  new_expr$type <- nCompiler:::type2symbol("numericVector")
  if (length(inserts) == 0) NULL else inserts
}

model_values_indexed_LAT_impl <- function(code, symTab, auxEnv, info) {
  # convert model_values_indexed_(model, instrList, i), from values(model, nodes[i], mode = "node"),
  # to model->getValues_indexed_impl(instrList, i) or, on the LHS of an assignment,
  # setValues_indexed_(model, instrList, i), which returns a proxy whose operator= does the copy
  # (see modelClass_.h in nimbleModel).
  onLHS <- isTRUE(auxEnv$onLHS)
  # Label the index as an ordinary (RHS) expression, even when the values() call is on the LHS.
  auxEnv$onLHS <- FALSE
  inserts <- nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(
    code, symTab, auxEnv, info, useArgs = c(FALSE, FALSE, TRUE)
  )
  auxEnv$onLHS <- onLHS
  iExpr <- nCompiler:::removeArg(code, 3)
  modelName <- as.name(code$args[[1]]$name)
  instrListName <- as.name(code$args[[2]]$name)
  if (onLHS) {
    new_code <- substitute(
      setValues_indexed_(MODEL, INSTRLIST, I),
      list(MODEL = modelName, INSTRLIST = instrListName, I = 0L) # placeholder for the labeled index
    )
  } else {
    new_code <- substitute(
      M(MODEL, "getValues_indexed_impl", INSTRLIST, I),
      list(M = as.name("->method"), MODEL = modelName, INSTRLIST = instrListName, I = 0L)
    )
  }
  new_expr <- nCompiler::nParse(new_code)
  nCompiler:::setArg(new_expr, if (onLHS) 3 else 4, iExpr)
  nCompiler:::replaceArgInCaller(code, new_expr)
  if (onLHS) nCompiler:::compile_normalizeCalls(new_expr, symTab, auxEnv)
  new_expr$type <- nCompiler:::type2symbol("numericVector")
  if (length(inserts) == 0) NULL else inserts
}

model_values_indexed_LAT <- function(code, symTab, auxEnv, info) {
  # See getOrSetValues_LAT for why this calls an implementation function.
  nimble2:::model_values_indexed_LAT_impl(code, symTab, auxEnv, info)
}

model_values_nodes_LAT_impl <- function(code, symTab, auxEnv, info) {
  # convert model_values_nodes_(model, instrList), from values(model, nodes, mode = "node"),
  # to model->getValues_nodes_impl(instrList) or, on the LHS of an assignment,
  # setValues_nodes_(model, instrList), which returns a proxy as for setValues_indexed_.
  onLHS <- isTRUE(auxEnv$onLHS)
  modelName <- as.name(code$args[[1]]$name)
  instrListName <- as.name(code$args[[2]]$name)
  if (onLHS) {
    new_code <- substitute(setValues_nodes_(MODEL, INSTRLIST),
                           list(MODEL = modelName, INSTRLIST = instrListName))
  } else {
    new_code <- substitute(M(MODEL, "getValues_nodes_impl", INSTRLIST),
                           list(M = as.name("->method"), MODEL = modelName, INSTRLIST = instrListName))
  }
  new_expr <- nCompiler::nParse(new_code)
  nCompiler:::replaceArgInCaller(code, new_expr)
  if (onLHS) nCompiler:::compile_normalizeCalls(new_expr, symTab, auxEnv)
  new_expr$type <- nCompiler:::type2symbol("numericVector")
  NULL
}

model_values_nodes_LAT <- function(code, symTab, auxEnv, info) {
  # See getOrSetValues_LAT for why this calls an implementation function.
  nimble2:::model_values_nodes_LAT_impl(code, symTab, auxEnv, info)
}

getOrSetValues_indexed_LAT <- function(code, symTab, auxEnv, info) {
  # See getOrSetValues_LAT for why this calls an implementation function.
  nimble2:::getOrSetValues_indexed_LAT_impl(code, symTab, auxEnv, info)
}

getOrSetValues_LAT <- function(code, symTab, auxEnv, info) {
  # The current function is baked into the follow list.
  # For debugging, we then call an implementation function that can be
  # dynamically worked on.
  nimble2:::getOrSetValues_LAT_impl(code, symTab, auxEnv, info)
}

MethodCall_LAT_impl <- function(code, symTab, auxEnv, info) {
  # nCompiler parses A$foo(x) to MethodCall(A, foo(x)).
  # We intercept nimbleListGen$new(...) and otherwise defer to nCompiler's MethodCall handler.
  # This replaces nimble's keyword processing of nimbleListGen$new(...), 
  # (temporarily, only during compileNimble), and it uses nCompiler's
  # scoping to find the nimbleList generator.
  #
  # The work is done by modifying the AST (code) in place.
  LATenv <- nCompiler:::labelAbstractTypesEnv
  if(code$args[[2]]$name == "new" && code$args[[1]]$isName) {
    # Label the type of A (but not new(...)) to see if A is a nimbleList generator.
    # Labeling a name does not create inserts.
    LATenv$recurse_labelAbstractTypes(code, symTab, auxEnv, info,
                                      useArgs = c(TRUE, FALSE))
    objType <- code$args[[1]]$type
    if(inherits(objType, "symbolNCgenerator") && is.nlGenerator(objType$NCgenerator)) {
      newNimbleList_LAT(code, symTab, auxEnv, info)
      return(NULL)
    }
  }
  # Return whatever nCompiler's handler returns, as if it were called directly.
  LATenv$MethodCall(code, symTab, auxEnv, info)
}

MethodCall_LAT <- function(code, symTab, auxEnv, info) {
  # See getOrSetValues_LAT for why this calls an implementation function.
  nimble2:::MethodCall_LAT_impl(code, symTab, auxEnv, info)
}

newNimbleList_LAT <- function(code, symTab, auxEnv, info) {
  # This does some major AST engineering, modifying code in place.
  # We enter with MethodCall(listDef, new(...)), with listDef already labeled
  # as a nimbleList generator. The nClass for a nimbleList has only a default
  # constructor, so we create the object and then set any fields given as arguments.
  NCgen <- code$args[[1]]$type$NCgenerator
  classname <- nCompiler::NCinternals(NCgen)$cpp_classname

  NC_info <- nCompiler:::register_known_nClass(NCgen, project_env = auxEnv$project_env)

  returnSym <- nCompiler:::symbolNC$new(name = '',
                            type = code$args[[1]]$type$name,
                            isArg = FALSE,
                            overloadDefs = NC_info$inheritInfo$overloadDefs,
                            NCgenerator = NCgen)

  # 1. Match the arguments of new(...) to the nimbleList fields.
  #    This names any positional arguments and catches unknown field names.
  newCode <- code$args[[2]]
  fieldMatchDef <- makeNimbleListTemplateWithBlankFirstArg(nl.getListDef(NCgen))
  formals(fieldMatchDef) <- formals(fieldMatchDef)[-1] # remove .LEFTSIDE
  nCompiler:::exprClass_put_args_in_order(fieldMatchDef, newCode)

  # 2. Change to construct_new_nClass(), as nCompiler does for Gen$new() (see MethodCall in
  #    nCompiler's labelAbstractTypes). nClass_constructor generates nClass_builder<classname>()(args),
  #    taking the class name from aux$cpp_classname, so all args are removed.
  # The careful way to extract args, removing them from the AST
  numSetterLines <- length(newCode$args)
  setterVarNames <- names(newCode$args)
  setterArgs <- vector("list", length = numSetterLines)
  for(i in rev(seq_len(numSetterLines))) {
    setterArgs[[i]] <- nCompiler:::removeArg(newCode, i)
  }
  nCompiler:::removeArg(code, 2) # new(...)
  nCompiler:::removeArg(code, 1) # the listDef
  code$name <- 'construct_new_nClass'
  code$aux$cpp_classname <- classname
  code$type <- returnSym
  newCode <- code

  # 3. Build a lambda function
  lambdaCodeOp <- nCompiler:::nParse(quote(chainedCall(LambdaFun_(decl, def))))
  #.   Place it in the AST in place of construct_new_nClass()
  nCompiler:::replaceArgInCaller(newCode, lambdaCodeOp)
  lambdaCodeOp$type <- returnSym

  # Set the decl argument to a lambda function opening string
  lambdaCode <- lambdaCodeOp$args[[1]]
  lambdaDecl <- nCompiler:::nParse(as.name(paste0('[&]()')))
  nCompiler:::setArg(lambdaCode, 1, lambdaDecl)
  
  tempName <- as.name(nimble2:::intermediateLabelMaker())
  # Set the body to create the new object, using the
  #. construct_new_nClass() piece from above
  lambdaBody <- nCompiler:::nParse(substitute({TEMP <- dummy; return(TEMP)}, 
                                              list(TEMP = tempName)))
 # lambdaBody <- nCompiler:::nParse(quote(return(dummy)))
  nCompiler:::setArg(lambdaBody$args[[1]], 2, newCode)
  # Add additional lines for any setting:
  if(numSetterLines) {
    for(i in 1:numSetterLines) {
      newLine <- substitute(TEMP$V <- value, 
                            list(TEMP = tempName, V = as.name(setterVarNames[i])))
      newLine <- nCompiler:::nParse(newLine)
      nCompiler:::setArg(newLine, 2, setterArgs[[i]])
      nCompiler:::insertArg(lambdaBody, 2, newLine)
    }
  }
  
  # process the body. Since it is a `{`, any inserts would be attached to its lines, not returned.
  nCompiler:::compile_labelAbstractTypes(lambdaBody, symTab, auxEnv)
  # put the body in place in the lambda fun call.
  nCompiler:::setArg(lambdaCode, 2, lambdaBody)
  # All work is done by modifying the AST in place.
  invisible(NULL)
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
  model_getParam = list(matchDef = function(model, instr, paramID) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_getBound = list(matchDef = function(model, instr, boundID) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_getLogProb = list(matchDef = function(model, instrList) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_calculate_indexed = list(matchDef = function(model, instrList, i) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_calculateDiff_indexed = list(matchDef = function(model, instrList, i) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_simulate_indexed = list(matchDef = function(model, instrList, i) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_getLogProb_indexed = list(matchDef = function(model, instrList, i) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_getParam_indexed = list(matchDef = function(model, instrList, i, paramID) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  model_getBound_indexed = list(matchDef = function(model, instrList, i, boundID) {}, simpleTransformations = list(handler = make_model_calls_methods)),
  getOrSetValues_ = list(matchDef = function(multiCopier) {},
    labelAbstractTypes = list(handler = getOrSetValues_LAT)),
  getOrSetValues_indexed_ = list(matchDef = function(multiCopier, i, mode) {},
    labelAbstractTypes = list(handler = getOrSetValues_indexed_LAT)),
  model_values_indexed_ = list(matchDef = function(model, instrList, i) {},
    labelAbstractTypes = list(handler = model_values_indexed_LAT)),
  # A C++ template function in nimbleModel's modelClass_.h, emitted by model_values_indexed_LAT.
  setValues_indexed_ = list(
    matchDef = function(model, instrList, i) {},
    labelAbstractTypes = list(handler = "custom_call",
                              recurse = TRUE,
                              returnType = "numericVector")),
  model_values_nodes_ = list(matchDef = function(model, instrList) {},
    labelAbstractTypes = list(handler = model_values_nodes_LAT)),
  # A C++ template function in nimbleModel's modelClass_.h, emitted by model_values_nodes_LAT.
  setValues_nodes_ = list(
    matchDef = function(model, instrList) {},
    labelAbstractTypes = list(handler = "custom_call",
                              recurse = TRUE,
                              returnType = "numericVector")),
  nimCopy_ = list(
    matchDef = function(from, fromRow, to, toRow) {},
    labelAbstractTypes = list(handler = "custom_call",
                              recurse = TRUE,
                              returnType = nCompiler:::symbolVoid$new())),
  # Registered with modify = TRUE (the default), so only the labelAbstractTypes
  # handler is replaced and nCompiler's cppOutput handler is kept.
  MethodCall = list(labelAbstractTypes = list(handler = MethodCall_LAT))
)

#' @importFrom nCompiler registerOpDef deregisterOpDef
#' @importFrom nimbleModel modelValues modelValuesConf
#' @export
compileNimble <- function(..., project, dirName = NULL, projectName = "",
                          control = list(),
                          resetFunctions = FALSE,
                          reset = FALSE,
                          showCompilerOutput = getNimbleOption("showCompilerOutput")) {
  ## 1. Extract compilation items
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
  # A project is one independent compilation: within it, each object has one compiled
  # counterpart, but the same object can be compiled in other projects too.
  # To compile objects together with objects already compiled, provide the project argument.
  # reset = TRUE: compile the inputs in a new project.
  # project provided: use it (it can be a project or any object compiled in one, in
  #   which case it is the project that most recently compiled the object).
  # Otherwise: use the nimbleProject option if set, or else a new project.
  # With nimbleOptions(legacy_nimble = TRUE), as in nimble, models already compiled are
  #   shared: if models among the inputs or used in their setup code were compiled,
  #   the project that most recently compiled them is used, even if the project argument
  #   is a different project (with a note).
  if (reset) {
    if (!missing(project)) {
      stop("reset = TRUE compiles the inputs in a new project, so the project argument should not be provided.", call. = FALSE)
    }
    project <- nimbleProjectClass$new(name = projectName)
  } else {
    if (!missing(project)) {
      project <- getNimbleProject(project, TRUE)
    } else {
      project <- NULL
    }
    if (isLegacyNimble()) {
      models_projects <- nimbleProject_registry_lookup_models(units)
      if (length(models_projects) > 1) {
        stop(paste0(
          "With nimbleOptions(legacy_nimble = TRUE), models already compiled are shared, but models provided for compilation,",
          " or used in their setup code, were compiled in different nimbleProjects, so they can't be compiled together."
        ), call. = FALSE)
      }
      if (length(models_projects) == 1) {
        if (!is.null(project) && !identical(project, models_projects[[1]])) {
          messageIfVerbose(paste0(
            "  [Note] Because nimbleOptions(legacy_nimble = TRUE), compiling in the project where models provided\n",
            "         (or used in setup code) were already compiled, instead of the project argument."
          ))
        }
        project <- models_projects[[1]]
      }
    }
    if (is.null(project)) {
      if (!is.null(getNimbleOption("nimbleProject"))) {
        project <- getNimbleOption("nimbleProject")
      } else {
        project <- nimbleProjectClass$new(name = projectName) # is dirName needed?
      }
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
      ans[[i]] <-
        project$RCfunction_add(units[[i]], control = control)
      if (names(units)[i] != "") names(ans)[i] <- names(units)[i]
    }
  }

  # N.B. modelValues are not allowed as units for compileNimble.
  # They can only be included as members from setup code.

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

  nlUnits <- unitTypes == 'nl'
  if(sum(nlUnits) > 0) {
    # In nimble it superficially looks like nimbleLists are supported as
    # objects for compilation, but actually that was never fully built out
    # and results in an error.
    stop("nimbleList objects are not supported for direct compilation. They are supported if created in setup code of nimbleFunctions.")
    # whichUnits <- which(nlUnits)
    # for (i in whichUnits) {
    #   ans[[i]] <- project$nimbleList_add(units[[i]], control = control)
    #   if (names(units)[i] != "") names(ans)[i] <- names(units)[i]
    # }  
  }
  # From here we are ready to:
  # Have the project create the nfProcs
  # Collect compilation units
  # Call nCompile
  # Build and populate objects

  if (isTRUE(.GlobalEnv$BROWSE_COMPILE_NIMBLE)) browser()

  project$process()
  # Only units not already compiled in this project are collected.
  nComp_units <- project$get_nComp_units()

  # names(nComp_units) <- names(units)
  nCompile_results <- list()
  if (length(nComp_units)) {
    nCompiler::registerOpDef(nimble_nCompiler_opDefs)
    on.exit({
      nCompiler::deregisterOpDef(ls(nimble_nCompiler_opDefs))
    }, add = TRUE)
    nCompile_results <- do.call(nCompiler::nCompile, c(nComp_units, list(returnList = TRUE)))
  }

  if (isTRUE(.GlobalEnv$BROWSE_COMPILE_NIMBLE)) browser()

  compiled_units <- vector("list", length = length(units))

  # Only objects that do not yet have compiled counterparts are instantiated and populated.
  project$instantiate_and_populate(nCompile_results)

  # simple functions (rcf) do not need instantiation.
  if (sum(rcfUnits) > 0) {
    whichUnits <- which(rcfUnits)
    for (i in whichUnits) {
      compiled_units[[i]] <- project$RCfunction_getResults(units[[i]])
    }
  }

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

  # No need to check for nlUnits because they are not really supported
  # as compileNimble inputs.

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

# Return the nimbleProject associated with project, which can be a nimbleProject
# or any object compiled in one: a model, nimbleFunction instance, modelValues
# or nimbleList object, or a compiled counterpart of one of these.
# For an uncompiled object, this is the project that most recently compiled it.
getNimbleProject <- function(project, stopOnNull = FALSE) {
  if (inherits(project, "nimbleProjectClass")) {
    return(project)
  }
  ans <- nimbleProject_registry_lookup(project)
  if (is.null(ans) && stopOnNull) {
    stop(paste0(
      "Cannot determine a nimbleProject from the project argument. It should be a nimbleProject,",
      " or a model or nimbleFunction (or compiled version of one) that has already been compiled."
    ), call. = FALSE)
  }
  ans
}
