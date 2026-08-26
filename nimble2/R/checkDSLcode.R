
n2_update_and_check_RCfun_code <- function(f,
                                           check = FALSE,
                                           methodNames = NULL,
                                           setupVarNames = NULL,
                                           buildDerivs = FALSE,
                                           where) {
  # In nimble this is done in nfMethodRC$initialize
  code <- nf_changeNimKeywords(body(f))
  if (code[[1]] != "{") {
    code <- substitute(
      {
        CODE
      },
      list(CODE = code)
    )
  }
  body(f) <- code
  arguments <- formals(f)
  if (check && "package:nimble2" %in% search()) {
    nf_checkDSLcode(code, methodNames, setupVarNames, names(arguments), where)
    if (isTRUE(nimbleOptions("doADerrorTraps"))) {
      if (!isFALSE(buildDerivs) && !is.null(buildDerivs)) {
        nf_checkDSLcode_derivs(code, names(arguments), callsNotAllowedInAD)
      }
    }
  }
  f
}

nf_checkDSLcode <- function(code, methodNames, setupVarNames, args, where = NULL) {
  validCalls <- c(
    nimbleSizeCallNames, # see note in keywords.R.
    otherDSLcalls,
    names(specificCallReplacements),
    nimKeyWords,
    methodNames,
    setupVarNames
  )
  calls <- setdiff(
    all.names(code),
    c(all.vars(code), args)
  )

  ## Find the 'y' in cases of x$y() and x[]$y() and x[[]]$y().

  # nfMethods <- findMethodsInExprClass(RparseTree2ExprClasses(code))
  nfMethods <- findMethodsInRcode(code)

  ## don't check RHS of $ to ensure it is a valid nf method because no current way to easily find the methods of nf's defined in setup code
  nonDSLcalls <- calls[!(calls %in% c(validCalls, nfMethods))]
  if (length(nonDSLcalls)) {
    objInR <- sapply(nonDSLcalls, exists, where = where)
    nonDSLnonR <- nonDSLcalls[!objInR]
    nonDSLinR <- nonDSLcalls[objInR]
    if (length(nonDSLinR)) {
      ## nf and nimbleFunctionList cases probably will never
      ## occur as these need to be passed as setup args or
      ## created in setup
      ##
      ## problem with passing inputIsName when run through roxygen...
      nonDSLinR <-
        nonDSLinR[!(sapply(
          nonDSLinR,
          function(x) {
            is.nf(x, inputIsName = TRUE, where = where)
          }
        ) |
          sapply(
            nonDSLinR,
            function(x) {
              is(get(x, envir = where), "nimbleFunctionList")
            }
          ) |
          sapply(
            nonDSLinR,
            function(x) {
              is.rcf(x, inputIsName = TRUE, where = where)
            }
          ) |
          sapply(
            nonDSLinR,
            function(x) {
              is.nlGenerator(x, inputIsName = TRUE, where = where)
            }
          ))]
    }
    if (length(nonDSLinR)) {
      message("  [Note] Detected possible use of R functions in nimbleFunction run code.\n         For this nimbleFunction to compile, '", paste(nonDSLinR, collapse = ", "), "' must be defined as a nimbleFunction, nimbleFunctionList, or nimbleList.")
    }
    if (length(nonDSLnonR)) {
      message("  [Note] For this nimbleFunction to compile, '", paste(nonDSLnonR, collapse = ", "), "' must be defined as a nimbleFunction, nimbleFunctionList, or nimbleList before compilation.")
    }
  }
  return(0)
}

# This replaces findMethodsInExprClass in nimble
# That used exprClasses, creating a separate parsing and 
# AST creation just for the checking. We think
# the task is simple enough to do it in a pure R
# syntax tree here instead.
findMethodsInRcode <- function(code) {
  if(is.name(code)) return(NULL)
  if(is.numeric(code)) return(NULL)
  if(is.logical(code)) return(NULL)
  if(is.character(code)) return(NULL)
  fxn_expr <- code[[1]]
  # Always recurse. If there are no args, then
  # res will be NULL
  res <- lapply(code[-1], findMethodsInRcode) |> unlist()
  if(is.name(fxn_expr)) {
    # We have a simple call, foo(arg1, arg2), so just return res
    return(res)
  }
  # We have a chained call, like obj$foo(arg1, arg2)
  # fxn_expr is the `obj$foo` part, so pull that apart next
  fxn_expr_op <- fxn_expr[[1]]
  if(identical(fxn_expr_op, as.name("$"))) {
    method_name <- deparse(fxn_expr[[3]])
    # recursion allows a$b(x)$c(y) to find both c and b.
    return(c(method_name, findMethodsInRcode(fxn_expr[[2]]), res))
  }
  # We have some unknown case, so return NULL
  NULL
}

nf_checkDSLcode_buildDerivs <- function(code, buildDerivs) {
  code <- body(code)
  codeNames <- all.names(code)
  derivsLocn <- which(codeNames %in% c("derivs", "nimDerivs"))
  if (length(derivsLocn)) {
    for (i in seq_along(derivsLocn)) {
      if (!(length(codeNames) >= derivsLocn[i] + 3 &&
        codeNames[derivsLocn[i] + 1] == "$" &&
        codeNames[derivsLocn[i] + 3] == "calculate")) {
        #
        methodName <- codeNames[derivsLocn[i] + 1]
        if (isFALSE(buildDerivs) ||
          !length(buildDerivs) ||
          is.null(buildDerivs) ||
          (is.character(buildDerivs) && !methodName %in% buildDerivs) ||
          (is.list(buildDerivs) && !methodName %in% names(buildDerivs))) {
          #
          messageIfVerbose(
            "  [Note] Detected use of `nimDerivs` with a function or method, `",
            methodName,
            "`, for which `buildDerivs` has not been set. This nimbleFunction cannot be compiled."
          )
        }
      }
    }
  }
  invisible(NULL)
}
