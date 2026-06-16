.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  ns$labelFunctionCreator <- labelFunctionMetaCreator()
  ns$nf_refClassLabelMaker <- labelFunctionCreator("nfRefClass")
  ns$projectNameCreator <- labelFunctionCreator('P')
  # Rname2CppName is not exported by nCompiler, but nCompiler is by the same author team
  # so we are confident in maintainability of using it here.
  ns$Rname2CppName <- getFromNamespace("Rname2CppName", "nCompiler")
}
