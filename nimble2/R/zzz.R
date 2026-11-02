.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  ns$labelFunctionCreator <- labelFunctionMetaCreator()
  ns$nf_refClassLabelMaker <- labelFunctionCreator("nfRefClass")
  ns$intermediateLabelMaker <- labelFunctionCreator("Interm_")
  ns$projectNameCreator <- labelFunctionCreator("P")
  # Registry of the nimbleProject that most recently compiled each object. See nimbleProjectRegistry.R.
  ns$nimbleProject_registry <- new.env(parent = emptyenv())
  # Rname2CppName is not exported by nCompiler, but nCompiler is by the same author team
  # so we are confident in maintainability of using it here.
  ns$Rname2CppName <- getFromNamespace("Rname2CppName", "nCompiler")
}
