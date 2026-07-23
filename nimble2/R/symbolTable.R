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

symbolInstrList <- 
  R6::R6Class(
    classname = "symbolInstrList",
    inherit = nCompiler:::symbolBase,
    public =
      list(
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
