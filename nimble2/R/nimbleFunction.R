#' Class \code{nimbleFunctionBase}
#' @aliases nimbleFunctionBase
#' @export
#' @description
#' Classes used internally in NIMBLE and not expected to be called directly by users.
nimbleFunctionBase <- setRefClass(
  Class = "nimbleFunctionBase",
  fields = list(
    .generatorFunction = "ANY",
    .CobjectInterface = "ANY"#,
#    .newSetupLinesProcessed = "ANY"
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
    } # ,
    # getDefinition in nimble appears never used, so removing it from nimble2
    #                                      getDefinition = function()
    #                                          nimble:::getDefinition(.self)
  )
) # 	$runRelated

#' @export
nimbleFunctionVirtual <- function(contains = NULL,
                                  run = function() { },
                                  methods     = list(),
                                  name        = NA,
                                  methodControl = list()) {
  # We simply pack methodControl into a list to pass into the ... 
  # arg of nimbleFunction. This will be used to update the
  # compileInfo virtual and abstract settings for the method nFunctions.
  # We must also set the compileInfo$createFromR to FALSE if there are any
  # abstract methods. This is done in nfProcessing$build_NCgenerator
  virtual <- list(methodControl = methodControl)
  nf <- nimbleFunction(
    setup = TRUE,
    run = run,
    methods = methods,
    contains = contains,
    name = name,
    virtual = virtual)
  nf
}

#' @importFrom nCompiler nFunction
#' @export
nimbleFunction <- function(setup = NULL,
                           run = function() {},
                           methods = list(),
                           globalSetup = NULL,
                           contains = NULL,
                           buildDerivs = list(),
                           name = NA,
                           check = getNimbleOption("checkNimbleFunction"),
                           where = parent.frame(), # getNimbleFunctionEnvironment()
                           ... # ... is added to nimble2 to support nimbleFunctionVirtual
) {
  force(where) # so that we can get to namespace where a nf is defined by using topenv(parent.frame(2)) in getNimbleFunctionEnvironment()
  if (is.logical(setup)) if (setup) setup <- function() {} else setup <- NULL


  ## Check for correct entries in `buildDerivs` separately from `nfMethodRC$new()` because
  ## that only has access to `thisBuildDerivs`, and we need to check if `buildDerivs` is set
  ## for the method on which `nimDerivs` is called.
  tmp <- sapply(c(list(run = run), methods), nf_checkDSLcode_buildDerivs, buildDerivs)

  ## Check that if a model calculate is in the code of `run` or another method on
  ## which `derivs` is called, that the `model`, `updateNodes`,and `constantNodes`
  ## arguments are provided.
  if (getNimbleOption("checkDerivsArgs") && length(buildDerivs)) {
    allMethods <- c(list(run = run), methods)
    if (is.character(buildDerivs)) nms <- buildDerivs else nms <- names(buildDerivs)
    methodsWithCalc <- sapply(allMethods[nms], nf_checkDSLcode_checkForCalc)
    methodsWithCalc <- nms[methodsWithCalc]
    methodsDerivsOf <- sapply(allMethods, nf_checkDSLcode_checkDerivsOf)
    methodsDerivsOf <- methodsDerivsOf[!sapply(methodsDerivsOf, is.null)]
    if (length(methodsWithCalc)) {
      tmp <- sapply(c(list(run = run), methods), nf_checkDSLcode_calcDerivsArgs, methodsWithCalc, methodsDerivsOf)
    }
  }

  if (is.null(setup)) {
    if (length(methods) > 0) stop('Cannot provide multiple methods if there is no setup function.  Use "setup = function(){}" or "setup = TRUE" if you need a setup function that does not do anything', call. = FALSE)
    if (!is.null(contains)) stop('Cannot provide a contains argument if there is no setup function.  Use "setup = function(){}" or "setup = TRUE" if you need a setup function that does not do anything', call. = FALSE)
    thisBuildDerivs <- FALSE
    if (isTRUE(getNimbleOption("enableDerivs"))) {
      if (isTRUE(buildDerivs)) buildDerivs <- list(run = list()) ## empty list means TRUE with no configuration information
      if (isFALSE(buildDerivs)) buildDerivs <- list()
      if (identical(buildDerivs, "run")) buildDerivs <- list(run = list())
      thisBuildDerivs <- buildDerivs[["run"]]
      if (is.null(thisBuildDerivs)) thisBuildDerivs <- FALSE
    }
    run <- n2_update_and_check_RCfun_code(
      run,
      check = check,
      buildDerivs = thisBuildDerivs, where = where
    )
    return(RCfunction(
      run,
      name = name,
      buildDerivs = thisBuildDerivs, where = where
    ))
  }

  virtual <- FALSE # used in nimbleProject

  dotsList <- list(...)
  dotsVirtual <- dotsList$virtual
  is_NFV <- FALSE # NFV = nimbleFunctionVirtual
  methodControl <- NULL
  if(!is.null(dotsVirtual)) {
    is_NFV <- TRUE
    virtual <- TRUE
    methodControl <- dotsVirtual$methodControl
  }

  if (isTRUE(getNimbleOption("enableDerivs")) && isTRUE(buildDerivs)) {
    stop("'buildDerivs' cannot be 'TRUE' when a setup function is provided. Please specify the specific method(s) for which 'buildDerivs' should be set.")
  }

  # we now include the namespace in the name of the RefClass to avoid two nfs having RefClass of same name but existing in different namespaces
  if (is.na(name)) name <- nf_refClassLabelMaker(envName = environmentName(where))
  className <- name
  methodList <- c(list(run = run), methods) # create a list of the run function, and all other methods
  # simply pass in names of vars in setup code so that those can be used in nf_checkDSLcode; to be more sophisticated we would only pass vars that are the result of nimbleListDefs or nimbleFunctions
  if (isTRUE(getNimbleOption("enableDerivs")) &&
    length(buildDerivs) > 0) {
    ## convert buildDerivs to a format of name = list(controls...)
    if (is.character(buildDerivs)) {
      buildDerivs <- structure(
        lapply(buildDerivs, function(x) list()),
        names = buildDerivs
      )
    }
  } else if (!isTRUE(getNimbleOption("enableDerivs")) &&
    length(buildDerivs) > 0) {
    stop('To build nimbleFunction derivatives, you must first set "nimbleOptions(enableDerivs = TRUE)".')
  }
  origMethodList <- methodList
  methodList <- list()
  setupVarNames <- c(all.vars(body(setup)), names(formals(setup)))
  # We retain the following two lists so that nfProcessing
  # can construct its own copies of the RCfunctions.
  # This is necessary for scoping purposes, especially for the
  # nimbleList case that a nimbleListDef is created in setup code
  # and used as a type declaration. In that case, the environment
  # captured in the quosure for those declarations when we create
  # the RCfunction (nFunction) here will not see the needed
  # nimbleListDef (nClass). When nfProcessing works to create
  # an nClass from the nimbleFunction, it will have an environment
  # for those, since it will be working from realized instances.
  # It's a minor case but it occurs and has to be supported.
  updatedOrigMethodList <- list()
  buildDerivsList <- list()

  for (iM in seq_along(origMethodList)) {
    thisBuildDerivs <- FALSE
    if (getNimbleOption("enableDerivs") &&
      length(buildDerivs) > 0) {
      thisBuildDerivs <- !is.null(buildDerivs[[names(origMethodList)[iM]]])
    }
    updatedMethod <- n2_update_and_check_RCfun_code(
      origMethodList[[iM]],
      check = check,
      methodNames = names(origMethodList),
      setupVarNames = setupVarNames,
      buildDerivs = thisBuildDerivs,
      where = where
    )
    updatedOrigMethodList[[iM]] <- updatedMethod
    buildDerivsList[[iM]] <- thisBuildDerivs
    methodList[[iM]] <- RCfunction(updatedMethod,
      # name ?
      buildDerivs = thisBuildDerivs,
      where = where
    )
  }
  names(updatedOrigMethodList) <- names(origMethodList)
  names(buildDerivsList) <- names(origMethodList)
  names(methodList) <- names(origMethodList)
  NCcompileInfo <- list()

  if(is_NFV) {
    for(m in names(methodList)) {
      CI <- nCompiler::NFinternals(methodList[[m]])$compileInfo
      CI$virtual <- TRUE
      if(!isFALSE(methodControl[[m]]$required)) { #cf nimble's cppVirtualNimbleFunctionClass$processNFprof
        CI$abstract <- TRUE
        anyAbstract <- TRUE
      }
      nCompiler::NFinternals(methodList[[m]])$compileInfo <- CI
    }
  }

  ## record any setupOutputs declared by setupOutput()
  setupOutputsDeclaration <- nf_processSetupFunctionBody(
    setup,
    returnSetupOutputDeclaration = TRUE
  )
  declaredSetupOutputNames <-
    nf_getNamesFromSetupOutputDeclaration(setupOutputsDeclaration)
  rm(setupOutputsDeclaration)
  ## create the reference class definition

  nfRefClassDef <- nf_createRefClassDef(
    setup, methodList,
    className, globalSetup, declaredSetupOutputNames, contains
  )
  nfRefClass <- eval(nfRefClassDef)
  .namesToCopy <- nf_namesNotHidden(names(nfRefClass$fields()))
  .namesToCopyFromGlobalSetup <- intersect(
    .namesToCopy,
    if (!is.null(globalSetup)) nf_assignmentLHSvars(body(globalSetup)) else character(0)
  )
  .namesToCopyFromSetup <- setdiff(.namesToCopy, .namesToCopyFromGlobalSetup)
  ## create a list to hold all specializations (instances) of this nimble function.  The following objects are accessed in environment(generatorFunction) in the future
  ## create the generator function, which is returned from nimbleFunction()
  ## To-do: consider whether the nimbleFunctionVirtual case should return an empty proxy generator (returning NULL), as in nimble.
  generatorFunction <- eval(nf_createGeneratorFunctionDef(setup))
  force(contains) ## eval the contains so it is in this environment
  formals(generatorFunction) <- nf_createGeneratorFunctionArgs(setup, parent.frame())
  environment(generatorFunction) <- GFenv <- new.env()
  parent.env(GFenv) <- parent.frame()

  .globalSetupEnv <- new.env()
  if (!is.null(globalSetup)) {
    if (!is.function(globalSetup)) {
      stop("If globalSetup is not NULL, it must be a function", call. = FALSE)
    }
    if (!length(formals(globalSetup)) == 0) {
      stop("globalSetup cannot take input arguments", call. = FALSE)
    }
    eval(body(globalSetup), envir = .globalSetupEnv)
  }

  for (var in c(
    "generatorFunction", "nfRefClassDef", "nfRefClass",
    "setup", "run", "methods", "methodList", "updatedOrigMethodList", "buildDerivsList",
    "name", "className", "contains",
    "buildDerivs", "virtual", ".globalSetupEnv", ".namesToCopy",
    ".namesToCopyFromGlobalSetup", ".namesToCopyFromSetup",
    "declaredSetupOutputNames", "methodControl"
  )) {
    GFenv[[var]] <- get(var)
  }
  return(generatorFunction)
}
