# symbolNimbleSpecial serves as an intermediate class to make it easy
# to identify all nimble-specific symbols.
symbolNimbleSpecial <-
  R6::R6Class(
    classname = "symbolNimbleSpecial",
    inherit = nCompiler:::symbolBase,
    public =
      list(
        initialize = function(...) {
          super$initialize(...)
        },
        print = function() writeLines(paste("symbolNimbleSpecial", self$name)),
        genCppVar = function(...) {
          stop(paste("Error, you should not be generating a cppVar for symbolNimbleSpecial", self$name))
        }
      )
  )

symbolVarRangeList <- 
  R6::R6Class(
    classname = "symbolVarRangeList",
    inherit = symbolNimbleSpecial,
    public =
      list(
        initialize = function(...) {
          super$initialize(...)
          self$type <- "Ronly"
        },
        print = function() writeLines(paste("symbolVarRangeList", self$name)),
        genCppVar = function(...) {
          stop(paste("Error, you should not be generating a cppVar for symbolVarRangeList", self$name))
        }
      )
  )

symbolMemberFunction <-
  R6::R6Class(
    classname = "symbolMemberFunction",
    inherit = symbolNimbleSpecial,
    public =
      list(
        nFun = NULL, ## added so that we can access returnType and argument types (origLocalSymbolTable)
        initialize = function(nFun, ...) {
          super$initialize(...)
          self$nFun <- nFun
          self$type <- "Ronly"
        },
        print = function() writeLines(paste("symbolMemberFunction", self$name)),
        genCppVar = function(...) {
          stop(paste("Error, you should not be generating a cppVar for symbolMemberFunction", self$name))
        }
      )
  )

# I think this is mostly used as a tag during processing
# symbolNimbleSpecial means it does not propagate to C++
symbolNimbleListGenerator <- 
  R6::R6Class(
    classname = "symbolNimbleListGenerator",
    inherit = symbolNimbleSpecial,
    public =
      list(
        NCgenerator = NULL,
        initialize = function(NCgenerator, ...) {
          super$initialize(...)
          self$type <- "nimbleListGenerator"
          self$NCgenerator <- NCgenerator
        },
        print = function() writeLines(paste("symbolNimbleListGenerator", self$name)),
        genCppVar = function(...) {
          stop(paste("Error, you should not be generating a cppVar for symbolNimbleListGenerator", self$name))
        }
      )
  )
  
symbolNimbleList <- 
  R6::R6Class(
    classname = "symbolNimbleList",
    inherit = nCompiler:::symbolBase,
    public =
      list(
        NCgenerator = NULL,
        initialize = function(NCgenerator, ...) {
          super$initialize(...)
          self$type <- "nimbleList"
          self$NCgenerator <- NCgenerator
        },
        resolveSym = function(...) {
          nCgen <- self$NCgenerator
          if (is.null(nCgen)) {
            stop("While resolving symbol in symbolNimbleList: NCgenerator is NULL")
          }
          nCompiler:::symbolNC$new(
            name = self$name,
            isArg = self$isArg, # should always be FALSE because nimble does not support passing nimbleFunctions as args.
            type = nCompiler:::NCinternals(nCgen)$cpp_classname, # consistent with nCompiler:::symbolTBD$resolveSym.
            NCgenerator = nCgen
          )
        },
        print = function() writeLines(paste("symbolNimbleList", self$name)),
        genCppVar = function(...) {
          stop(paste("Error, you should not be generating a cppVar for symbolNimbleList", self$name))
        }
      )
  )

symbolNimbleFunction <-
  R6::R6Class(
    classname = "symbolNimbleFunction",
    inherit = nCompiler:::symbolBase,
    public =
      list(
        declaration = NULL,
        nfProc = NULL,
        initialize = function(nfProc, ...) {
          super$initialize(...)
          self$type <- nfGetDefVar(nfProc$nfGenerator, "name")
          self$nfProc <- nfProc
        },
        # build_NCgenerator_impl simply uses this symbol as the nClass declaration type.
        # During nCompile, the resolveSym method is called to self-replace.
        resolveSym = function(...) {
          # When the symbol is initialized, not all nfProcs will have their
          # nClass generators built. But by the time resolveSym is called (during nCompile), they should.
          nCgen <- self$nfProc$NCgenerator
          if (is.null(nCgen)) {
            stop("While resolving symbol: NCgenerator is NULL")
          }
          nCompiler:::symbolNC$new(
            name = self$name,
            isArg = self$isArg, # should always be FALSE because nimble does not support passing nimbleFunctions as args.
            type = nCompiler:::NCinternals(nCgen)$cpp_classname, # consistent with nCompiler:::symbolTBD$resolveSym.
            NCgenerator = nCgen
          )
        },
        print = function() writeLines(paste("symbolNimbleFunction", self$name)),
        genCppVar = function(...) {
          stop(paste("Error, you should not be generating a cppVar for symbolNimbleFunction", self$name))
        }
      )
  )

symbolNimbleFunctionList <- 
  R6::R6Class(
    classname = "symbolNimbleFunctionList",
    inherit = nCompiler:::symbolBase,
    public =
      list(
        base_nfProc = NULL,
        initialize = function(base_nfProc, ...) {
          super$initialize(...)
          self$type <- "nimbleFunctionList"
          self$base_nfProc <- base_nfProc
        },
        resolveSym = function(...) {
          base_NCgen <- self$base_nfProc$NCgenerator
          res <- nCompiler:::type2symbol(nCompiler::nList(base_NCgen()),
                                         name = self$name,
                                         isArg = self$isArg)
          res <- res$resolveSym(...)
          return(res)
        },
        print = function() writeLines(paste("symbolNimbleFunctionList", self$name)),
        genCppVar = function(...) {
          stop(paste("Error, you should not be generating a cppVar for symbolNimbleFunctionList", self$name))
        }
      )
  )

symbolInstrList <- 
  R6::R6Class(
    classname = "symbolInstrList",
    inherit = nCompiler:::symbolBase,
    public =
      list(
        # In this case, resolveSym won't do because we want to see the nList as a declaration.
        # build_nCgenerator_impl checks any symbols in the nimbleFunction symbolTable and
        # replaced with any declaration fields like the following:
        declaration = "nCompiler::nList(nimbleModel:::instr_nClass())",
        initialize = function(...) {
          super$initialize(
            ...
          )
        },
        print = function() writeLines(paste("symbolInstrList", self$name)),
        genCppVar = function(...) {
          stop(paste("Error, you should not be generating a cppVar for symbolInstrList", self$name))
        }
      )
  )

symbolModel <-
  R6::R6Class(
    classname = "symbolModel",
    inherit = nCompiler:::symbolNC,
    public =
      list(
        initialize = function(...) {
          super$initialize(
            NCgenerator = nimbleModel:::modelBase_nClass,
            ...
          )
          ## type == 'local' means it is defined in setupCode and so will need to have an object and be built
          ## type == 'Ronly' means it is a setupArg and may be a different type for different nimbleFunction specializations
          ##                 and it will be like a model in C++ code: not there except by extracted pointers inside of it
        }
      )
  )

symbolModelValues <-
  R6::R6Class(
    classname = "symbolModelValues",
    inherit = nCompiler:::symbolNC,
    public =
      list(
        initialize = function(...) {
          super$initialize(
            NCgenerator = nimbleModel:::modelValuesBase_nClass,
            ...
          )
        }
      )
  )

Dptr_Bracket_LAT <- function(code, symTab, auxEnv, handlingInfo) {
  nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
  code$type <- nCompiler:::type2symbol("numericScalar")
}
Dptr_Bracket_EIG <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
  nCompiler:::eigenizeEnv$eigenCast(code, 2, "integer")
  isAssign <- isTRUE(handlingInfo$isAssign)
   # We will revert to `<-`(LHS, RHS)
    if(isAssign) {
      caller <- code$caller
      callerArgID <- code$callerArgID
      nCompiler:::eigenizeEnv$revert_OpAssign(code, symTab, auxEnv, workEnv, handlingInfo)
      code <- caller$args[[callerArgID]] # now `<-`(LHS, RHS)
      code <- code$args[[1]] # LHS, which is the original `[` call
    }
  NULL
}

# This gives simple double* support.

symbolDptr <- R6::R6Class(
  classname = "symbolDptr",
  inherit = nCompiler:::symbolBase,
  public = list(
    initialize = function(...) {
      super$initialize(type = 'symbolDptr', ...)
      self$interface <- FALSE
      self$overloadDefs <- list(
        "[" = list(
          labelAbstractTypes = list(
            handler = Dptr_Bracket_LAT
          ),
          eigenImpl = list(
            handler = Dptr_Bracket_EIG
          ),
          cppOutput = list(
            handler = nCompiler:::genCppEnv$IndexingBracket
          )
        ),
        "[<-" = list(
          labelAbstractTypes = list(
            handler = Dptr_Bracket_LAT
          ),
          eigenImpl = list(
            handler = Dptr_Bracket_EIG,
            isAssign = TRUE
          ),
          cppOutput = list(
            handler = nCompiler:::genCppEnv$IndexingBracket
          )
        )
      )
    },
    shortPrint = function() "symbolDptr",
    uniqueID = function() "symbolDptr",
    print = function() writeLines(paste0(self$name, ": symbolDptr")),
    genCppVar = function() {
      nCompiler:::cppVarFullClass$new(baseType = "double",
                                      name = self$name,
                                      ptr = TRUE,
                                      ref = FALSE)
    }
  )
)
