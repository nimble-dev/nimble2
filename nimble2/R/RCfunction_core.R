## creates unique labels ('nfRefClass1') for the reference class names for nimbleFunctions
# nf_refClassLabelMaker <- labelFunctionCreator('nfRefClass') # moved to .onLoad()
# for use in DSL code check:


#' @importFrom nCompiler nFunction
#' @export
RCfunction <- function(f, name = NA,
                       buildDerivs = FALSE, where = parent.frame()) {
  if (is.na(name)) name <- character() # keep old default for consistency and re-do here
  nFun <- nCompiler::nFunction(
    name = name, # This might be helpful
    fun = f,
    where = where
  )
  old_env <- environment(nFun)
  environment(nFun) <- new.env(parent = old_env)
  environment(nFun)$nfMethodRCobject <- NULL # This is a marker used by `is.rcf` consistent with nimble.
  nFun
}
