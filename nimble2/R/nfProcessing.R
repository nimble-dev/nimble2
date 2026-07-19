virtualNFprocessing <- setRefClass("virtualNFprocessing",
  fields = list(
    name = "ANY", ## character
    setupSymTab = "ANY", ## symbolTable
    nfGenerator = "ANY", ## 'function',
    #    compileInfos = "ANY", ## list of RCfunctionCompileClass objects
    origMethods = "ANY", ## list of original methods
    origSetupOutputNames = "ANY", ## character vector of original setup output names
    updatedSetupOutputNames = "ANY",
    declaredSetupOutputNames = "ANY", ## character vector of setup output names declared by setupOutput()
    matchedCodes = "ANY",
    processedCodes = "ANY",
    #    RCfunProcs = "ANY", ## list of RCfunProcessing  or RCvirtualFunProcessing objects
    nimbleProject = "ANY", ## nimbleProjectclass object
    cppDef = "ANY" ## cppNimbleFunctionClass or cppVirtualNimbleFunctionClass object
  ),
  methods = list(
    show = function() {
      writeLines(paste0("virtualNFprocessing object ", name))
    },
    initialize = function(f = NULL, className, virtual = TRUE, project = NULL) {{
      force(f) # avoid warnings from recursive promise evaluation during debugging
      #      browser()
    }
    nimbleProject <<- project
    #   compileInfos <<- list()
    matchedCodes <<- list()
    processedCodes <<- list()
    #   RCfunProcs <<- list()

    # isNode <<- isNode

    if (!is.null(f)) { ## This allows successful default instantiation by R when defining nfProcessing below -- crazy.
      ## nfGenerator is allowed if it is a nimbleFunctionVirtual.
      if (is.nf(f) | is.nfGenerator(f)) {
        nfGenerator <<- nf_getGeneratorFunction(f)
      } else if (inherits(f, "list")) {
        if (length(unique(lapply(f, nfGetDefVar, "name"))) != 1) {
          stop("Error with list of instances not having same nfGenerator")
        }
        nfGenerator <<- nf_getGeneratorFunction(f[[1]])
      }
      if (missing(className)) {
        sf <- environment(nfGenerator)$name
        name <<- nCompiler::Rname2CppName(sf)
      } else {
        name <<- className
      }
      origMethods <<- nf_getMethodList(nfGenerator)
      origMethodNames <- names(origMethods)
      origSetupOutputNames <<- nf_getSetupOutputNames(nfGenerator)
      updatedSetupOutputNames <<- origSetupOutputNames
      declaredSetupOutputNames <<- getFunctionEnvVar(nfGenerator, "declaredSetupOutputNames")
      # To-do: Manage a full and complete set of operator names, keywords, etc.
      conflictedNames <- origMethodNames %in% names(nCompiler:::operatorDefEnv)
      # conflictedNames <- origMethodNames %in% c(
      #   names(nimble:::cppOutputCalls), names(nimble:::specificCallReplacements),
      #   binaryOperators, unaryOperators,
      #   reductionUnaryOperators, matrixSquareReductionOperators,
      #   reductionBinaryOperators, nonNativeEigenCalls,
      #   matrixFlipOperators, matrixSquareOperators,
      #   nimbleListReturningOperators, matrixSolveOperators
      # )
      if (any(conflictedNames)) {
        stop(
          "The name of the nimbleFunction method `",
          paste0(origMethodNames[conflictedNames], collapse = "`,"),
          "` conflicts with a function in the NIMBLE language (DSL); please use a different name"
        )
      }
      # nimble2: stopping here, turning off the following:
      # use_RCfunProcs <- FALSE
      # if (use_RCfunProcs) {
      #   RCfunProcs <<- list()
      #   for (i in seq_along(origMethods)) {
      #     RCname <- names(origMethods)[i]
      #     # if(isNode && strsplit(RCname, '_', fixed = TRUE)[[1]][1] == getCalcADFunName()) constFlag <- FALSE
      #     # else
      #     constFlag <- isNode
      #     RCfunProcs[[RCname]] <<-
      #       if (virtual) {
      #         RCvirtualFunProcessing$new(origMethods[[i]], RCname, const = constFlag)
      #       } else {
      #         RCfunProcessing$new(origMethods[[i]], RCname, const = constFlag)
      #       }
      #   }
      #   compileInfos <<- lapply(
      #     RCfunProcs,
      #     function(x) x$compileInfo
      #   )
      # }
    }},
    # setupLocalSymbolTables = function() {
    #   for (i in seq_along(RCfunProcs)) {
    #     RCfunProcs[[i]]$setupSymbolTables(parentST = setupSymTab, neededTypes = list(), nimbleProject = nimbleProject)
    #   }
    # },
    # doRCfunProcess = function(control = list(debug = FALSE, debugCpp = FALSE)) {
    #   for (i in seq_along(RCfunProcs)) {
    #     RCfunProcs[[i]]$process(debug = control$debug, debugCpp = control$debugCpp, debugCppLabel = name, doKeywords = FALSE, nimbleProject = nimbleProject)
    #   }
    # },
    addMemberFunctionsToSymbolTable = function() {
      for (i in seq_along(origMethods)) {
        thisName <- names(origMethods)[i]
        newSym <- symbolMemberFunction$new(name = thisName, nFun = origMethods[[i]])
        setupSymTab$addSymbol(newSym)
      }
    },
    process = function(control = list(debug = FALSE, debugCpp = FALSE)) {
      setupSymTab <<- symbolTable(parentST = NULL)
      addMemberFunctionsToSymbolTable()
      # setupLocalSymbolTables()
      # doRCfunProcess(control)
    }
  )
)

# Rname2CppName is needed from nCompiler and obtained in .onLoad
nfProcessing <- setRefClass("nfProcessing",
  contains = "virtualNFprocessing",
  fields = list(
    instances = "ANY", ## list of instances of the nimbleFunction to used for setup types and receive newSetupCode
    neededTypes = "ANY", ## list of symbols for non-trivial types that will be needed for compilation, such as derived models or modelValues
    neededObjectNames = "ANY", ## character vector of the names of objects such as models or modelValues that need to exist during C++ instantiation and population so their contents can be pointed to
    newSetupOutputNames = "ANY", ## character vector of names of objects created by newSetupCode from "keyword processing" (which also adds to this vector)
    updatedNewSetupOutputNames = "ANY",
    blockFromCppNames = "ANY", ## character vector of names of setup outputs that should not be propagated to C++
    newSetupCode = "ANY", ## list of lines of setup code populated by keyword processing
    newSetupCodeOneExpr = "ANY", ## all lines of new setup code put into one expression for evaluation
    instances_newSetupEnvs = "ANY",
    newFields = "ANY",
    keywordCaseIDs = "ANY",
    newInitCode = "ANY",
    cpp_init_ = "ANY",
    nClassGen = "ANY"
  ),
  methods = list(
    show = function() {
      writeLines(paste0("nfProcessing object ", name))
    },
    initialize = function(f = NULL, className, project) {
      force(f) # avoid warnings from recursive promise evaluation during debugging
      neededTypes <<- list()
      neededObjectNames <<- character()
      newSetupCode <<- list()
      if (!is.null(f)) {
      ## f must be a specialized nf, or a list of them
        if (missing(className)) {
          sf <- if (is.list(f)) nfGetDefVar(f[[1]], "name") else nfGetDefVar(f, "name")
          name <<- nCompiler::Rname2CppName(sf)
        } else {
          name <<- className
        }
        callSuper(f, name, virtual = FALSE, project = project)
        instances <<-
          if (inherits(f, "list")) {
            lapply(f, nf_getRefClassObject)
          } else {
            list(nf_getRefClassObject(f))
          }
        # list for envs for evaluating new setup code for instances:
        instances_newSetupEnvs <<- vector("list", length(instances))
      }
      newSetupOutputNames <<- character()
      updatedNewSetupOutputNames <<- character()
      blockFromCppNames <<- character()
      newSetupCode <<- list()
      newFields <<- list()
      keywordCaseIDs <<- character()
      newInitCode <<- list()
    },
    getSymbolTable = function() setupSymTab,
    getMethodInterfaces = function() origMethods,
    processKeywords_all = function() {},
    matchKeywords_all = function() {},
    doSetupTypeInference_processNF = function() {},
    makeTypeObject = function() {},
    replaceCall = function() {},
    evalNewSetupLines = function() {},
    makeNewSetupLinesOneExpr = function() {},
    evalNewSetupLinesOneInstance = function(instances, check = FALSE) {},
    setupTypesForUsingFunction = function() {},
    doSetupTypeInference = function() {},
    # clearSetupOutputs = function() {},
    build_cpp_init_ = function() {},
    build_nClassGen = function() {},
    # setupLocalSymbolTables = function() {
    #   for (i in seq_along(RCfunProcs)) {
    #     RCfunProcs[[i]]$setupSymbolTables(parentST = setupSymTab, neededTypes = neededTypes, nimbleProject = nimbleProject)
    #   }
    # },
    # collectRCfunNeededTypes = function() {
    #   for (i in seq_along(RCfunProcs)) {
    #     for (j in names(RCfunProcs[[i]]$neededRCfuns)) {
    #       if (is.null(neededTypes[[j]])) {
    #         neededTypes[[j]] <<- RCfunProcs[[i]]$neededRCfuns[[j]]
    #       }
    #     }
    #     ## could clear RCfunProc[[i]]$neededRCtypes, but instead will prevent them from being used at compilation
    #   }
    # },
    collect_nimDerivs_info = function() {
      newBuildDerivs <- list()
      for (i in seq_along(RCfunProcs)) {
        ADinfoNames <- RCfunProcs[[i]]$compileInfo$typeEnv[["ADinfoNames_calculate"]]
        if (!is.null(ADinfoNames)) {
          methodName <- RCfunProcs[[i]]$name
          if (is.character(methodName)) { ## not sure when it wouldn't be; this is defensive
            newBuildDerivs[[methodName]] <- list(calculate = TRUE)
          }
        }
      }
      if (length(newBuildDerivs)) {
        environment(nfGenerator)$buildDerivs <<- c(
          newBuildDerivs,
          environment(nfGenerator)$buildDerivs
        )
      }
      for (i in seq_along(RCfunProcs)) {
        new_ignore <- RCfunProcs[[i]]$compileInfo$typeEnv[[".new_ignore"]]
        if (length(new_ignore) > 0) {
          thisFunName <- names(RCfunProcs)[i]
          thisBuildDerivs <- environment(nfGenerator)$buildDerivs[[thisFunName]]
          if (!is.null(thisBuildDerivs)) {
            if (is.null(thisBuildDerivs$ignore)) {
              thisBuildDerivs$ignore <- character()
            }
            thisBuildDerivs$ignore <- unique(c(
              thisBuildDerivs$ignore,
              new_ignore
            ))
            environment(nfGenerator)$buildDerivs[[thisFunName]] <<- thisBuildDerivs
          }
        }
      }
    },
    addBaseClassTypes = function() {
      ## If this class has a virtual base class, we add it to the needed types here
      contains <- environment(nfGenerator)$contains
      if (!is.null(contains)) {
        className <- environment(contains)$className
        nfp <- nimbleProject$setupVirtualNimbleFunction(contains, fromModel = inModel)
        newSym <- symbolNimbleFunction(name = name, type = "nimbleFunctionVirtual", nfProc = nfp)
        if (!(className %in% names(neededTypes))) neededTypes[[className]] <<- newSym
      }
    },
    process = function(control = list(debug = FALSE, debugCpp = FALSE)) {
      ## Modifications to R code
      # debug <- control$debug
      # debugCpp <- control$debugCpp
      # if (!is.null(getNimbleOption("debugNFProcessing"))) {
      #   if (getNimbleOption("debugNFProcessing")) {
      #     debug <- TRUE
      #     control$debug <- TRUE
      #     writeLines("Debugging nfProcessing (nimbleOptions('debugRCfunProcessing') is set to TRUE)")
      #   }
      # }

      # if (debug) {
      #   print("setupSymTab")
      #   print(setupSymTab)

      #   writeLines("***** READY FOR replaceModelSingleValues *****")
      #   browser()
      # }
      if (inherits(setupSymTab, "uninitializedField")) {
        ## This step could have already been done if the types were needed by another nimbleFunction
        setupTypesForUsingFunction()
      }
      # if (debug) browser()
      makeNewSetupLinesOneExpr()

      evalNewSetupLines()

      # if (debug) {
      #   print("setupSymTab")
      #   print(setupSymTab)
      #   print("newSetupOutputNames")
      #   print(newSetupOutputNames)
      #   print("newSetupCode")
      #   print(newSetupCode)
      #   writeLines("***** READY FOR doSetupTypeInference *****")
      #   browser()
      # }
      build_cpp_init_()

      doSetupTypeInference(setupOrig = FALSE, setupNew = TRUE)

      # if (debug) {
      #   print("lapply(compileInfos, function(x) print(x$newLocalSymTab))")
      #   lapply(compileInfos, function(x) print(x$newLocalSymTab))
      #   writeLines("**** READY FOR RFfunProcessing *****")
      #   browser()
      # }

      # doRCfunProcess(control)

      # collectRCfunNeededTypes()

      if (isTRUE(getNimbleOption("enableDerivs"))) {
        collect_nimDerivs_info()
      }

      build_nClassGen()
      # if (debug) {
      #   print("done with RCfunProcessing")
      #
      # }
    }
  )
)

nfProcessing$methods(build_cpp_init_ = function() {
  init_function <- function() {}
  body(init_function) <- as.call(c(list(as.name("{")), .self$newInitCode))
  init_ <- nFunction(
    fun = init_function,
    compileInfo = list(constructor = FALSE) # This is not an actual constructor; showing that clearly here.
  )
  cpp_init_ <<- init_
})

nfProcessing$methods(build_nClassGen = function() {
  new_methods <- list()
  for (i in seq_along(origMethods)) {
    thisName <- names(origMethods)[i]
    new_methods[[thisName]] <- origMethods[[i]]
    nCompiler::NFinternals(new_methods[[thisName]]) <- nCompiler::NFinternals(origMethods[[i]])$clone()
    nCompiler::NFinternals(new_methods[[thisName]])$updateCode(processedCodes[[i]])
  }
  members <- setupSymTab$symbols
  for (mn in names(members)) {
    sym <- members[[mn]]
    if (inherits(sym, "symbolNimbleSpecial")) {
      members[[mn]] <- NULL
      next
    }
    if(!is.null(sym$declaration)) {
      members[[mn]] <- sym$declaration
      next
    }
    if(is.character(sym$type) && length(sym$type) > 0 && sym$type[1] == "Ronly") {
      members[[mn]] <- NULL
      next
    }
  }
  classname <- "make_this_random"
  initL <- list(cpp_init_ = cpp_init_)
  nClassGen <<- eval(substitute(
    nCompiler::nClass(
      classname = CLASSNAME,
      Cpublic = c(
        MEMBERS,
        METHODS
      ),
      env = environment(nfGenerator)
    ),
    list(
      MEMBERS = c(members, .self$newFields),
      METHODS = c(new_methods, initL),
      CLASSNAME = classname
    )
  ))
  nClassGen
})

nfProcessing$methods(evalNewSetupLines = function() {
  if (length(instances) == 0) {
    warning("No specialized instances of nimble function")
    return()
  }
  for (i in seq_along(instances)) {
    evalNewSetupLinesOneInstance(i)
  }
})

nfProcessing$methods(makeNewSetupLinesOneExpr = function() {
  newSetupCodeOneExpr <<- as.call(c(list(as.name("{")), newSetupCode))
})

# nfProcessing$methods(clearSetupOutputs = function(inst) {
#   for (i in nf_getSetupOutputNames(nfGenerator)) {
#     inst[[i]] <- NULL
#   }
#   for (i in newSetupOutputNames) {
#     inst[[i]] <- NULL
#   }
#   NULL
# })

nfProcessing$methods(evalNewSetupLinesOneInstance = function(i, check = FALSE) {
  newSetupLinesProcessed <- is.environment(instances_newSetupEnvs[[i]])
  if (check) {
    if (newSetupLinesProcessed) {
      return(invisible(NULL))
    }
  }
  if(!newSetupLinesProcessed) {
    instance <- instances[[i]]
    if (is.nf(instance)) instance <- nf_getRefClassObject(instance)
    instances_newSetupEnvs[[i]] <<- new.env(parent = instances[[i]])
  }
  ## Warning: this relies on the fact that although refClass environments are closed, we can
  ## eval in them and create new variables in them that way.
  eval(newSetupCodeOneExpr, envir = instances_newSetupEnvs[[i]])
  #instance$.newSetupLinesProcessed <- TRUE
})

nfProcessing$methods(setupTypesForUsingFunction = function() {
  if (inherits(setupSymTab, "uninitializedField")) {
    doSetupTypeInference(TRUE, FALSE)
    addMemberFunctionsToSymbolTable()
    addBaseClassTypes()
    matchKeywords_all()
    processKeywords_all()
    # setupLocalSymbolTables()
  }
})

nfProcessing$methods(doSetupTypeInference = function(setupOrig, setupNew) {
  if (!setupOrig & !setupNew) {
    warning("Weird, doSetupTypeInference was called with both setupOrig and setupNew FALSE.  Nothing to do.", call. = FALSE)
    return(NULL)
  }
  if (length(instances) == 0) {
    warning("No specialized instances of nimble function", call. = FALSE)
    return(NULL)
  }
  outputNames <- character()
  if (setupOrig) {
    setupSymTab <<- nCompiler:::symbolTableClass$new()
    # setupSymTab$addSymbol(symbolNimbleFunctionSelf(
    #   name = ".self",
    #   nfProc = .self
    # ))
    outputNames <- c(outputNames, origSetupOutputNames) #nf_getSetupOutputNames(nfGenerator))
    if (length(outputNames) > 0) outputNames <- unique(outputNames)
  }
  if (setupNew) {
    ## Kluge that results from adding string handling to the compiler:
    ## Previously any character objects were assigned a symbol object with
    ## type 'Ronly'.  In later processing all 'Ronly' types are filtered out of
    ## propagation to C++.
    ## Now that we have added string handling, character objects are assigned
    ## a symbolString symbol with type "character" and not automatically filtered.
    ## Unfortunately this means that vectors of node names that are only used
    ## in lines like calculate(model, nodeNames), which undergoes keyword processing
    ## would be propogated to C++ wastefully.
    ## As a kluge, we will step in here, during second round of setup type inference
    ## to re-assign type 'Ronly' to any symbols that, as a result of
    ## keyword processing, we can now see are not needed
    ## We also need the section added below to filter out newSetupOutputs
    ## that are really created as intermediates for others that are really needed
    ## during the keyword processing, the newSetupOutputNames is used for
    ## bookkeeping, so it would not be trivial to remove them at an earlier stage.
    origSetupOutputs <- origSetupOutputNames #nf_getSetupOutputNames(nfGenerator)
    declaredSetupOutputs <- declaredSetupOutputNames # getFunctionEnvVar(nfGenerator, "declaredSetupOutputNames")
    origSetupOutputs <- setdiff(origSetupOutputs, declaredSetupOutputs)
    newRcodeList <- c(processedCodes, list(nCompiler::NFinternals(cpp_init_)$code))
    # newRcodeList <- lapply(compileInfos, `[[`, "newRcode")
    allNamesInCodeAfterKeywordProcessing <- unique(unlist(lapply(newRcodeList, all.names)))

    # We allow a keyword processor to include an orig setup output name in its "new"
    # setup output names to flag that it should be kept.
    # We need to identify those, mark them as needed, and not re-process them below.
    newSetupOutputNamesInOrig <- intersect(newSetupOutputNames, origSetupOutputs)

    origSetupOutputNamesToKeep <- intersect(
                              c(allNamesInCodeAfterKeywordProcessing, newSetupOutputNamesInOrig),
                              origSetupOutputs) ## this loses mv!


    origSetupOutputNamesNotNeeded <- setdiff(origSetupOutputs, origSetupOutputNamesToKeep)
    for (nameNotNeeded in origSetupOutputNamesNotNeeded) {
      thisSym <- setupSymTab$getSymbol(nameNotNeeded)
      if (!is.null(thisSym)) {
        if (!thisSym$type == "Values") {
          thisSym$type <- "Ronly"
        }
      } ## must keep modelValues, nimbleFunctions, possibly others
    }
    updatedSetupOutputNames <<- c(origSetupOutputNamesToKeep, declaredSetupOutputNames) |> unique()

    # newSetupOutputNames will have been populated during keyword processing.
    newSetupOutputNamesToProcess <- setdiff(newSetupOutputNames, newSetupOutputNamesInOrig)
    updatedNewSetupOutputNames <<- newSetupOutputNamesToProcess
    outputNames <- c(outputNames, newSetupOutputNamesToProcess)
  }
  doSetupTypeInference_processNF(setupSymTab, outputNames, add = TRUE) # add info about each setupOutput to symTab

  if (setupNew) {
    ## This is the second part of the kluge.
    ## Probably it would be ok to never add these to the symbol table in the first place
    ## but right now I am doing it this way to minimize unforeseen consequences by more closely mimicing what would have been created prior to adding string support
    ## This is trickier because keyword processing can create objects for propogation to C++ that never appear in method code (e.g. manyVariableAccessors used to construct copierVectors)
    ## So I added a blockFromCppNames that is populated during keyword processing

    for (nameNotNeeded in blockFromCppNames) {
      thisSym <- setupSymTab$getSymbol(nameNotNeeded)
      if (!is.null(thisSym)) thisSym$type <- "Ronly"
    }
  }
})

nfProcessing$methods(doSetupTypeInference_processNF = function(symTab,
      setupOutputNames, add = FALSE, firstOnly = isTRUE(getNimbleOption("deduceTypeFromFirstInstanceOnly"))) {
  if (length(instances) == 0) {
    warning("Can not infer setup output types with no instances.")
    return(invisible(NULL))
  }
  for (name in setupOutputNames) {
    symbolRCobject <- makeTypeObject(name, firstOnly)
    if (is.null(symbolRCobject)) next
    if (is.logical(symbolRCobject)) {
      stop(paste0("There is an error involving the type of ", name, "."), call. = FALSE)
    }
    if (add) symTab$addSymbol(symbolRCobject)
  }
})


nfProcessing$methods(getModelVarDim = function(modelVarName, labelVarName, firstOnly = FALSE) {
  firstNDim <- instances[[1]][[modelVarName]]$modelDef$varInfo[[labelVarName]]$nDim
  if (!firstOnly) {
    if (!all(unlist(lapply(instances, function(x) x[[modelVarName]]$modelDef$varInfo[[labelVarName]]$nDim == firstNdim)))) {
      warning(paste0("Problem: not all instances of label ", labelVarName, " in model ", modelVarName, " have the same number of dimensions."))
      return(invisible(NULL))
    }
  }
  return(firstNDim)
})

## firstOnly is supposed to indicate whether we look at only the first instance, or use all of them
## but actually, right now, we use it inconsistently.
## this is a function that could use a lot of polishing, but it's ok for now.
nfProcessing$methods(makeTypeObject = function(name, firstOnly = FALSE) {
  makeTypeObj_impl(.self, name, firstOnly)
})

makeTypeObj_impl <- function(.self, name, firstOnly) {
  is_newSetupOutput <- name %in% .self$newSetupOutputNames
  if (!is_newSetupOutput) {
    instances_to_use <- .self$instances
  } else {
    instances_to_use <- .self$instances_newSetupEnvs
  }
  first_inst <- instances_to_use[[1]][[name]]
  # isNLG <- FALSE
  # if (is.nlGenerator(instances[[1]][[name]])) {
  #   nlGen <- instances[[1]][[name]]
  #   isNLG <- TRUE
  # } else if (exists(name, envir = globalenv())) {
  #   foundObject <- get(name, envir = globalenv())
  #   if (is.nlGenerator(foundObject)) {
  #     nlGen <- foundObject
  #     isNLG <- TRUE
  #   }
  # }
  # if (isNLG) {
  #   nlp <- .self$nimbleProject$compileNimbleList(nlGen, initialTypeInferenceOnly = TRUE)
  #   className <- nl.getListDef(nlGen)$className
  #   newSym <- symbolNimbleList(name = name, nlProc = nlp)
  #   .self$neededTypes[[className]] <- newSym ## if returnType is a NLG, this will ensure that it can be found in argType2symbol()
  #   returnSym <- symbolNimbleListGenerator(name = name, nlProc = nlp)
  #   return(returnSym)
  # }
  # if (is.nl(instances[[1]][[name]])) {
  #   ## This case mimics the nimbleFunction case below (see is.nf)

  #   ## We need all instances created in setup code from all instances
  #   nlList <- lapply(instances, `[[`, name)
  #   ## trigger initial procesing to set up an nlProc object
  #   ## that will have a symbol table.
  #   ## Issue: We may also need to trigger this step from run code
  #   nlp <- .self$nimbleProject$compileNimbleList(nlList, initialTypeInferenceOnly = TRUE)
  #   ## get the unique name that we use to generate a unique C++ definition
  #   className <- nlList[[1]]$nimbleListDef$className
  #   ## add the setupOutput name to objects that we need to instantiate and point to
  #   .self$neededObjectNames <- c(.self$neededObjectNames, name)

  #   ## create a symbol table object
  #   newSym <- symbolNimbleList(name = name, nlProc = nlp)

  #   ## If this is the first time this type is encountered,
  #   ## add it to the list of types whose C++ definitions will need to be generated
  #   if (!(className %in% names(.self$neededTypes))) .self$neededTypes[[className]] <- newSym
  #   return(newSym)
  # }
  # if (inherits(instances[[1]][[name]], "indexedNodeInfoTableClass")) {
  #   return(symbolIndexedNodeInfoTable(name = name, type = "symbolIndexedNodeInfoTable")) ## the class type will get it copied but the Ronly will make it skip a type declaration, which is good since it is in the nodeFun base class.
  # }
  # if (inherits(instances[[1]][[name]], "nimbleFunctionList")) {
  #   .self$neededObjectNames <- c(.self$neededObjectNames, name)
  #   baseClass <- instances[[1]][[name]]$baseClass ## an nfGenerator created by virtualNimbleFunction()
  #   baseClassName <- environment(baseClass)$className

  #   if (!(baseClassName %in% names(.self$neededTypes))) {
  #     nfp <- .self$nimbleProject$setupVirtualNimbleFunction(baseClass, fromModel = .self$inModel)
  #     newSym <- symbolNimbleFunctionList(name = name, type = "nimbleFunctionList", baseClass = baseClass, nfProc = nfp)
  #     neededTypeSim <- symbolNimbleFunction(name = baseClassName, type = "virtualNimbleFunction", nfProc = nfp)
  #     .self$neededTypes[[baseClassName]] <- newSym
  #   } else {
  #     newSym <- .self$neededTypes[[baseClassName]]
  #   }

  #   allInstances <- unlist(lapply(instances, function(x) x[[name]]$contentsList), recursive = FALSE)
  #   newNFprocs <- .self$nimbleProject$compileNimbleFunctionMulti(allInstances, initialTypeInference = TRUE)
  #   ## only types are needed here, not initialTypeInference, because nfVar's from a nimbleFunctionList are not available (could be in future)
  #   for (nfp in newNFprocs) {
  #     newTypeName <- environment(nfp$nfGenerator)$name
  #     .self$neededTypes[[newTypeName]] <- symbolNimbleFunction(
  #       name = newTypeName, type = "nimbleFunction",
  #       nfProc = nfp
  #     )
  #   }
  #   return(newSym)
  # }
  if (is.nf(first_inst)) { ## nimbleFunction
    funList <- lapply(instances_to_use, `[[`, name)
    nfp <- .self$nimbleProject$nimbleFunction_add(funList) ## will return existing nfProc if it exists
    # className <- class(nf_getRefClassObject(funList[[1]]))
    # .self$neededObjectNames <- c(.self$neededObjectNames, name)
    newSym <- symbolNimbleFunction$new(name = name, type = "nimbleFunction", nfProc = nfp)
    # if (!(className %in% names(.self$neededTypes))) .self$neededTypes[[className]] <- newSym
    return(newSym)
  }
  # if (inherits(instances[[1]][[name]], "modelValuesBaseClass")) { ## In some cases these could be different derived classes.  If locally defined they must be the same
  #   if (!firstOnly) {
  #     if (!all(unlist(lapply(instances, function(x) inherits(x[[name]], "modelValuesBaseClass"))))) {
  #       warning(paste0("Problem: some but not all instances have ", name, " as a modelValues.  Types must be consistent."))
  #       return(invisible(NULL))
  #     }
  #   }
  #   ## Generate one set of symbolModelValues objects for the neededTypes, and each of these can have its own mvConf
  #   ## Generate another symbolModelValues to return and have in the symTab for this compilation
  #   ## I don't think that mvConf gets used, since they all get Values *
  #   for (i in seq_along(instances)) {
  #     className <- class(instances[[i]][[name]])
  #     if (!(className %in% names(.self$neededTypes))) {
  #       ## these are used only to build neededTypes
  #       ntSym <- symbolModelValues(name = name, type = "Values", mvConf = instances[[i]][[name]]$mvConf)
  #       .self$neededTypes[[className]] <- ntSym
  #     }
  #   }
  #   ## this is used in the symbol table
  #   .self$neededObjectNames <- c(.self$neededObjectNames, name)
  #   newSym <- symbolModelValues(name = name, type = "Values", mvConf = NULL)
  #   return(newSym)
  # }
  if (inherits(first_inst, "modelBase_nClass")) {
    if (!firstOnly) {
      if (!all(unlist(lapply(instances_to_use, function(x) inherits(x[[name]], "modelBase_nClass"))))) {
        warning(paste0("Problem: some but not all instances have ", name, " as a model.  Types must be consistent."))
        return(invisible(NULL))
      }
      # if (!all(unlist(lapply(instances, function(x) inherits(x[[name]], "RmodelBaseClass"))))) {
      #   warning(paste0("Problem: models should be provided as R model objects, not C model objects"))
      #   return(invisible(NULL))
      # }
    }
    .self$nimbleProject$model_add(instances_to_use[[1]][[name]])
    return(symbolModel$new(
      name = name,
      type = "modelBase_nClass",
      isArg = FALSE
    )) # previously used a className, but may not be needed
  }
  if(is.list(first_inst) &&
     length(first_inst) > 0 &&
      inherits(first_inst[[1]], "varRangeClass")) {
        return(symbolVarRangeList$new(name = name))
  }
  if(inherits(first_inst, "nList")) {
    return(symbolInstrList$new(name = name))
  }
  # if (inherits(instances[[1]][[name]], "ADproxyModelClass")) {
  #   if (!isTRUE(getNimbleOption("enableDerivs"))) {
  #     stop("It looks like derivatives are being created but nimbleOptions('enableDerivs') is not TRUE.")
  #   }
  #   return(symbolModel(name = name, type = "Ronly", className = class(instances[[1]][[name]]$model)))
  # }

  # if (inherits(instances[[1]][[name]], "NumericListBase")) {
  #   varinfo <- instances[[1]][[name]]
  #   if (!firstOnly) {
  #     if (!all(unlist(lapply(instances, function(x) inherits(x[[name]], "NumericListBase"))))) {
  #       warning(paste0("Problem: some but not all instances have ", name, " as a NumericList.  Types must be consistent."))
  #       return(invisible(NULL))
  #     }
  #   }

  #   return(symbolNumericList(name = name, type = varinfo$listType, nDim = max(varinfo$nDim, 1),                                                    = class(instances[[1]][[name]])))
  # }

  # if (inherits(instances[[1]][[name]], "copierVectorClass")) {
  #   newSym <- symbolCopierVector(name = name, type = "symbolCopierVector")
  #   return(newSym)
  # }

  # if (inherits(instances[[1]][[name]], "singleVarAccessClass")) {
  #   ## Keeping this simple: only doing first instance for now
  #   varInfo <- instances[[1]][[name]]$model$getVarInfo(instances[[1]][[name]]$var)
  #   ## Maybe we should intercept this case in the model, but for now here:
  #   if (instances[[1]][[name]]$useSingleIndex) {
  #     nDim <- 1
  #     size <- prod(varInfo$maxs)
  #   } else {
  #     nDim <- varInfo$nDim
  #     size <- varInfo$maxs
  #     if (length(nDim) == 0) browser()
  #     if (is.na(nDim)) browser()
  #     if (nDim == 0) {
  #       nDim <- 1
  #       size <- 1
  #     } ## There is no such thing as a scalar in a model
  #   }
  #   return(symbolNimArrDoublePtr(name = name, type = "double", nDim = nDim, size = size))
  # }

  # if (inherits(instances[[1]][[name]], "singleModelValuesAccessClass")) {
  #   varOrgName <- instances[[1]][[name]]$var
  #   varSym <- instances[[1]][[name]]$modelValues$symTab$getSymbolObject(varOrgName)
  #   nDim <- max(c(varSym$nDim, 1))
  #   type <- instances[[1]][[name]]$modelValues$symTab$symbols[[varOrgName]]$type
  #   return(symbolVecNimArrPtr(name = name, type = type, nDim = nDim, size = varSym$size))
  # }

  # if (inherits(instances[[1]][[name]], "nodeFunctionVector")) {
  #   return(symbolNodeFunctionVector(name = name))
  # }
  # if (inherits(instances[[1]][[name]], "nodeFunctionVector_nimDerivs")) {
  #   return(symbolNodeFunctionVector_nimDerivs(name = name))
  # }
  # if (inherits(instances[[1]][[name]], "modelVariableAccessorVector")) {
  #   return(symbolModelVariableAccessorVector(name = name, lengthName = paste0(name, "_length")))
  # }
  # if (inherits(instances[[1]][[name]], "modelValuesAccessorVector")) {
  #   return(symbolModelValuesAccessorVector(name = name))
  # }
  # if (inherits(instances[[1]][[name]], "getParam_info")) { ## the paramInfo in an instance is allowed to be NULL (see GitHub Issue #327). Hence we search for the first valid case and default to double()
  #   iInst <- 1
  #   paramInfo <- instances[[iInst]][[name]]
  #   while (is.na(paramInfo$type) & iInst < length(instances)) {
  #     iInst <- iInst + 1
  #     paramInfo <- instances[[iInst]][[name]]
  #   }
  #   if (is.na(paramInfo$type)) paramInfo <- defaultParamInfo()
  #   return(symbolGetParamInfo(name = name, paramInfo = paramInfo))
  # }
  # if (inherits(instances[[1]][[name]], "getBound_info")) {
  #   return(symbolGetBoundInfo(name = name, boundInfo = instances[[1]][[name]]))
  # }
  if (is.character(first_inst)) {
    if (firstOnly) {
      nDim <- if (is.null(dim(first_inst))) 1L else length(dim(first_inst[[name]]))
      if (nDim > 1) {
        warning("character object with nDim > 1 being handled as a vector")
        nDim <- 1
      }
      size <- if (length(first_inst) == 1) 1L else as.numeric(NA)
      if (getNimbleOption("convertSingleVectorsToScalarsInSetupArgs")) {
        if (nDim == 1 & identical(as.integer(size), 1L)) nDim <- 0
      }
      return(nCompiler:::symbolBasicString$new(name = name, nDim = nDim))
    } else {
      instanceObjs <- lapply(instances_to_use, `[[`, name)
      types <- unlist(lapply(instanceObjs, storage.mode))
      if (!all(types == "character")) stop(paste("Inconsistent types for setup variable", name))
      dims <- lapply(instanceObjs, dim)
      dimsNULL <- unlist(lapply(dims, is.null))
      if (any(dimsNULL)) { ## dimsNULL TRUE means it is a vector
        if (!all(dimsNULL)) {
          warning(paste0("Dimensions do no all match for ", name, "but they will be treated as all vectors anyway."))
        }
      }
      nDim <- 1
      lengths <- unlist(lapply(instanceObjs, length))
      size <- if (!all(lengths == 1)) as.numeric(NA) else 1L
      if (getNimbleOption("convertSingleVectorsToScalarsInSetupArgs")) {
        if (nDim == 1 & identical(as.integer(size), 1L)) nDim <- 0
      }
      return(nCompiler:::symbolBasicString$new(name = name, nDim = nDim))
    }
  }
  if(nCompiler:::isNC(first_inst)) {
    if(is.null(attr(first_inst, "NCgenerator")))
      stop("nClass objects created in setup code or by partial evaluation (keyword processing) must have an 'NCgenerator' attribute set.")
    NCgen <- attr(first_inst, "NCgenerator")
    if(!nCompiler:::isNCgenerator(NCgen))
      NCgen <- eval(NCgen, envir = instances_to_use[[1]])
    if(!nCompiler:::isNCgenerator(NCgen))
      stop("nClass objects created in setup code or by partial evaluation (keyword processing) must have an 'NCgenerator' attribute set to an nClass generator object or code that evaluates to one.")
    return(nCompiler:::symbolNC$new(name = name,
        type = nCompiler:::NCinternals(NCgen)$cpp_classname,
        isArg = FALSE,
        NCgenerator = NCgen))
  }
  if (is.numeric(first_inst) || is.logical(first_inst)) {
    if (firstOnly) {
      type <- storage.mode(first_inst)
      nDim <- if (is.null(dim(first_inst))) 1L else length(dim(first_inst))
      size <- if (length(first_inst) == 1) rep(1L, nDim) else rep(as.numeric(NA), nDim)
      declared_type <- attr(first_inst, "nimble_type")
      if (getNimbleOption("convertSingleVectorsToScalarsInSetupArgs")) {
        if (nDim == 1 & identical(as.integer(size), 1L)) nDim <- 0
      }
      if(!is.null(declared_type)) {
        type <- declared_type$type %||% type
        nDim <- declared_type$nDim %||% nDim
       # size <- declared_type$size %||% size
      }
      return(nCompiler:::symbolBasic$new(name = name, type = type, nDim = nDim, size = size))
    } else {
      instanceObjs <- lapply(instances_to_use, `[[`, name)
      types <- unlist(lapply(instanceObjs, storage.mode))
      dims <- lapply(instanceObjs, \(x) if (is.null(dim(x))) length(x) else dim(x)) |> unlist()
#      dimsNULL <- unlist(lapply(dims, is.null))
      declared_types <- lapply(instanceObjs, function(x) attr(x, "nimble_type"))
      bool_declared_types <- unlist(lapply(declared_types, function(x) !is.null(x)))
      if (any(bool_declared_types)) {
        use_declaredTypes <- TRUE
        declared_typeList <- lapply(declared_types, function(x) x$type)
        declared_nDimList <- lapply(declared_types, function(x) x$nDim)
       # declared_sizeList <- lapply(declared_types, function(x) x$size)
      } else {
        use_declaredTypes <- FALSE
      }

      default_singleton_nDim <- if(getNimbleOption("convertSingleVectorsToScalarsInSetupArgs")) 0 else 1
      nDims <- lapply(dims, function(x) if (length(x) == 1 && x[1] == 1) default_singleton_nDim else length(x))
      if(use_declaredTypes) {
        bool_declared_nDimList <- lapply(declared_nDimList, \(x) !is.null(x)) |> unlist()
        nDims[bool_declared_nDimList] <- declared_nDimList[bool_declared_nDimList]
      }
      if(length(unique(nDims)) > 1) {
        warning(paste0("Problem, dimensions do no all match for ", name))
        return(NA)
      }
      nDim <- nDims[[1]]
      size <- rep(as.numeric(NA), nDim) # not really used in nimble2 and may be removed
      if(nDim == 0) size <- 1L

      if(use_declaredTypes) {
        bool_declared_typeList <- lapply(declared_typeList, \(x) !is.null(x)) |> unlist()
        types[bool_declared_typeList] <- declared_typeList[bool_declared_typeList]
      }

      if (any(types == "double")) {
        if (!all(types %in% c("double", "integer"))) {
          warning("Problem: some but not all instances have ", name, " as double or integer.  Types must be consistent.")
          return(NA)
        }
        return(nCompiler:::symbolBasic$new(name = name, type = "double", nDim = nDim, size = size))
      }
      if (any(types == "integer")) {
        if (!all(types == "integer")) {
          warning("Problem: some but not all instances have ", name, " as integer.  Types must be consistent.")
          return(NA)
        }
        return(nCompiler:::symbolBasic$new(name = name, type = "integer", nDim = nDim, size = size))
      }
      if (any(types == "logical")) {
        if (!all(types == "logical")) {
          warning("Problem: some but not all instances have ", name, " as logical.  Types must be consistent.")
          return(NA)
        }
        return(nCompiler:::symbolBasic$new(name = name, type = "logical", nDim = nDim, size = size))
      }
    }
  }
  return(NA)
}

nfProcessing$methods(determineNdimsFromInstances = function(modelExpr, varOrNodeExpr) {
  allNDims <- lapply(instances, function(x) {
    model <- eval(modelExpr, envir = x)
    if (!exists(as.character(varOrNodeExpr), x, inherits = FALSE)) {
      stop(paste0("Error, ", as.character(varOrNodeExpr), " does not exist in an instance of this nimbleFunction."))
    }
    lab <- eval(varOrNodeExpr, envir = x)
    varAndIndices <- getVarAndIndices(lab)
    determineNdimFromOneCase(model, varAndIndices)
  })
  return(allNDims)
})

nfProcessing$methods(processKeywords_all = function() {
  for (i in seq_along(origMethods)) {
    processedCodes[[i]] <<-
      processKeywords_recurse(
        matchedCodes[[i]], .self,
        nCompiler::NFinternals(origMethods[[i]])
      )
  }
})

nfProcessing$methods(matchKeywords_all = function() {
  for (i in seq_along(origMethods)) {
    matchedCodes[[i]] <<-
      matchKeywords_recurse(nCompiler::NFinternals(origMethods[[i]])$code, .self)
  }
})
