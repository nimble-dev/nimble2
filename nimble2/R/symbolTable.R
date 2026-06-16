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

# nimSymbolTable <-
#   R6::R6Class(
#     Class = "nimSymbolTable",
#     public = list(
#       symbols = NULL,
#       parentST = NULL,
#       initialize = function(parentST = NULL) {
#         symbols <<- list()
#         parentST <<- parentST
#       },
#       addSymbol = function(symbolRCobject, allowReplace = FALSE) {
#         ##  if(!is(symbolRCobject, 'symbolBase'))   stop('adding non-symbol object to symbolTable')
#         name <- symbolRCobject$name
#         if (!allowReplace)
#           if (name %in% getSymbolNames())
#             warning(paste0("duplicate symbol name: ", name))
#         symbols[[name]] <<- symbolRCobject
#       },
#       ## remove a symbol RC object from this symbolTable; gives warning if symbol isn't in table
#       removeSymbol = function(name) {
#         if (!(name %in% getSymbolNames())) warning(paste0("removing non-existant symbol name: ", name))
#         symbols[[name]] <<- NULL
#       },

#       ## symbol accessor functions
#       getLength = function() {
#         return(length(symbols))
#       },
#       getSymbolObjects = function() {
#         return(symbols)
#       },
#       getSymbolNames = function() if (is.null(names(symbols))) {
#         return(character(0))
#       } else {
#         return(names(symbols))
#       },
#       getSymbolObject = function(name, inherits = FALSE) {
#         ans <- symbols[[name]]
#         if (is.null(ans))
#           if (inherits)
#             if (!is.null(parentST))
#               ans <- parentST$getSymbolObject(name, TRUE)
#         return(ans)
#       },
#       symbolExists = function(name, inherits = FALSE) {
#         return(!is.null(getSymbolObject(name, inherits)))
#       },
#       ## parentST accessor functions
#       getParentST = function() {
#         return(parentST)
#       },
#       setParentST = function(ST) parentST <<- ST,
#       print = function() {
#         writeLines("symbol table:")
#         for (i in seq_along(symbols)) symbols[[i]]$print()
#         if (!is.null(parentST)) {
#           writeLines("parent symbol table:")
#           parentST$print()
#         }
#       }
#     )
#   )
