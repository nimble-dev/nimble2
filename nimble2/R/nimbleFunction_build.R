## processing of all objects to become NF member data
## needs to be exported as otherwise use of nimble::: in `nf_createGeneratorFunctionDef()` gives R CMD check NOTE
#' @export
nf_preProcessMemberDataObject <- function(obj) {
  if (inherits(obj, "CmodelBaseClass")) {
    warning("This nimbleFunction was passed a *compiled* model object.\nInstead, the corresponding *uncompiled* model object was used.", call. = FALSE)
    return(obj$Rmodel)
  }
  return(obj)
}

## definition for the nimble function generator (specializer)
nf_createGeneratorFunctionDef <- function(setup) {
  generatorFunctionDef <- substitute(
    function() {
      SETUPCODE # execute setupCode
      nfRefClassObject <- nfRefClass() # create an object of the reference class
      nfRefClassObject$.generatorFunction <- generatorFunction # link upwards to get the generating function of this nf
      ## assign setupOutputs into reference class object
      if (!getNimbleOption("compileOnly")) {
        for (.var_unique_name_1415927 in .namesToCopyFromGlobalSetup) {
          nfRefClassObject[[.var_unique_name_1415927]] <- nf_preProcessMemberDataObject(get(.var_unique_name_1415927, envir = .globalSetupEnv))
        }
      }
      for (.var_unique_name_1415927 in .namesToCopyFromSetup) {
        nfRefClassObject[[.var_unique_name_1415927]] <- nf_preProcessMemberDataObject(get(.var_unique_name_1415927))
      }
      return(nfRefClassObject)
    },
    list(SETUPCODE = nf_processSetupFunctionBody(setup, returnCode = TRUE))
  )
  generatorFunctionDef[[4]] <- NULL
  return(generatorFunctionDef)
}

nf_processSetupFunctionBody <- function(
  setup,
  returnCode = FALSE,
  returnSetupOutputDeclaration = FALSE
) {
  code <- body(setup)
  returnLineNum <- 0
  for (i in seq_along(code)) {
    if (is.call(code[[i]])) {
      if (is.name(code[[i]][[1]])) {
        if (code[[i]][[1]] == "setupOutputs") {
          returnLineNum <- i
          break
        }
      }
    }
  }
  if (sum(all.names(code) == "setupOutputs") > 1) {
    stop("multiple setupOutputs() declarations in nimbleFunction setup argument; only one allowed")
  }
  if (returnLineNum == 0) {
    ## no setupOutputs() declaration found; default behavior
    setupOutputDeclaration <- quote(setupOutputs())
  } else {
    ## setupOutputs() declaration was found
    setupOutputDeclaration <- code[[returnLineNum]]
    code[returnLineNum] <- NULL
  }
  if ("list" %in% all.names(setupOutputDeclaration)) stop("setupOutputs(...) declaration should not include 'list()'")
  if (returnCode) {
    return(code)
  }
  if (returnSetupOutputDeclaration) {
    return(setupOutputDeclaration)
  }
  stop("must specify either returnCode=TRUE or returnSetupOutputDeclaration=TRUE")
}

nf_getNamesFromSetupOutputDeclaration <- function(setupOutputsDeclaration) {
  if (setupOutputsDeclaration[[1]] != "setupOutputs") {
    stop("something went wrong")
  }
  return(
    unlist(
      lapply(
        setupOutputsDeclaration[-1],
        function(so) {
          if (is.call(so)) {
            stop("cannot have a call inside setupOutputs() declaration")
          } else {
            deparse(so)
          }
        }
      )
    )
  )
}

## template for the reference class internal to all nimble functions
nf_createRefClassDef <- function(
  setup, methodList,
  className = nf_refClassLabelMaker(),
  globalSetup,
  declaredSetupOutputNames,
  contains = NULL
) {
  finalMethodList <- methodList # previously nfMethodRC used to generate callable here.
  finalMethodList[["show"]] <- eval(substitute(
    function() writeLines(paste0("reference class object for nimble function class ", className)),
    list(className = className)
  ))
  if (!is.null(contains)) {
    finalMethodList <- c(
      finalMethodList, nf_getBaseClassMethods(methodList, contains)
    )
  }
  substitute(
    setRefClass(
      Class = NFREFCLASS_CLASSNAME,
      fields = NFREFCLASS_FIELDS,
      methods = NFREFCLASS_METHODS,
      contains = "nimbleFunctionBase", # 	$runRelated
      where = where
    ),
    list(
      NFREFCLASS_CLASSNAME = className,
      NFREFCLASS_FIELDS =
        nf_createRefClassDef_fields(
          setup, methodList,
          globalSetup, declaredSetupOutputNames
        ),
      NFREFCLASS_METHODS = finalMethodList
    )
  )
}

nf_getBaseClassMethods <- function(methodList, contains) {
  contains_env <- environment(contains)
  baseClassMethodNames <- names(contains_env$methods)
  ## Including run seems to make this complete, although in fact missing run args to
  ## nimbleFunction will result in an empty function, so it won't be missing.
  if (is.function(contains_env$run)) {
    baseClassMethodNames <- c("run", baseClassMethodNames)
  }
  for (mn in baseClassMethodNames) {
    reqd <- TRUE # reqd FALSE means the method might be taken from contains
    if (is.logical(contains_env$methodControl[[mn]][["required"]])) {
      reqd <- contains_env$methodControl[[mn]][["required"]][1]
    }
    provided <- mn %in% names(methodList)
    if (!reqd) {
      if (!provided) {
        methodList[[mn]] <- contains_env$methods[[mn]]
      }
    }
    if (reqd) {
      if (!provided) {
        messageIfVerbose("  [Warning] method ", mn, " is required from the contains (base) class, but was not provided.")
      }
    }
  }
  methodList
}

## creates a list of the fields (setupOutputs) for a nimble function reference class
nf_createRefClassDef_fields <- function(
  setup, methodList,
  globalSetup, declaredSetupOutputNames
) {
  setupOutputNames <-
    nf_createSetupOutputNames(
      setup, methodList,
      declaredSetupOutputNames, globalSetup
    )
  if (FALSE) print(setupOutputNames)
  fields <- as.list(rep("ANY", length(setupOutputNames)))
  names(fields) <- setupOutputNames
  return(fields)
}

nf_createSetupOutputNames <- function(
  setup, methodList,
  declaredSetupOutputNames, globalSetup
) {
  setupOutputNames <- character(0)
  setupOutputNames <- c(setupOutputNames, names(formals(setup))) # add all setupArgs to potential setupOutputs
  setupOutputNames <- c(
    setupOutputNames, nf_assignmentLHSvars(body(setup)),
    if (!is.null(globalSetup)) nf_assignmentLHSvars(body(globalSetup)) else character()
  ) # add all variables on LHS of <- in setup to potential setupOutputs
  setupOutputNames <- intersect(
    setupOutputNames,
    nf_createAllNamesFromMethodList(methodList)
  )
  setupOutputNames <- c(setupOutputNames, declaredSetupOutputNames)
  setupOutputNames <- unique(setupOutputNames)
  return(setupOutputNames)
}

nf_assignmentLHSvars <- function(code) {
  if (!is.call(code)) {
    return(character(0))
  }
  isAssign <- code[[1]] == "<-" | code[[1]] == "="
  if (!isAssign) {
    return(
      unique(
        unlist(
          lapply(as.list(code), nf_assignmentLHSvars)
        )
      )
    )
  }
  if (isAssign) {
    return(
      c(
        nf_getVarFromAssignmentLHScode(code[[2]]),
        nf_assignmentLHSvars(code[[3]])
      )
    )
  }
}

## determines the name of the target variable, from the LHS code of an `<-` assignment statement
nf_getVarFromAssignmentLHScode <- function(code) {
  if (is.name(code)) {
    return(deparse(code))
  }
  return(nf_getVarFromAssignmentLHScode(code[[2]]))
}

## creates a list of all the names of all variables and functions in the code of methodList functions
nf_createAllNamesFromMethodList <- function(methodList, onlyArgsAndReturn = F) {
  methodBodyListCode <- list()
  if (!onlyArgsAndReturn) {
    methodBodyListCode <- lapply(methodList, function(f) body(f))
  } # f$code)
  methodReturnListCode <- list() # lapply(methodList, function(f) f$returnType) ##might need changing
  methodArgListCode <- list() # lapply(methodList, function(f) f$argInfo$argList[[1]])
  methodListCode <- c(methodBodyListCode, methodArgListCode, methodReturnListCode)
  if (length(methodListCode) > 0) {
    return(unique(unlist(lapply(methodListCode, function(code) all.names(code)))))
  }
}


## generates the argument list for the generator function
nf_createGeneratorFunctionArgs <- function(setup, pf) {
  generatorFunctionArgs <-
    lapply(
      formals(setup),
      function(arg) {
        if (is_blank(arg)) arg else eval(arg, pf)
      }
    )
  return(generatorFunctionArgs)
}

## returns names which don't begin with '.'
nf_namesNotHidden <- function(names) {
  names[!grepl("^\\.", names)]
}
