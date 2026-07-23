getVarAndIndices <- function(code) {
    if(is.character(code)) code <- parse(text = code, keep.source = FALSE)[[1]]
    if(length(code) > 1) {
        if(code[[1]] == '[') {
            varName <- code[[2]]
            indices <- as.list(code[-c(1,2)])
        } else {
            stop(paste('Error:', deparse(code), 'is a malformed node label.'))
        }
    } else {
        varName <- code
        indices <- list()
    }
    list(varName = varName, indices = indices)
}

indicesList2matrix <- function(indices) {
  extractRow <- function(x) {
    if(nCompiler:::is.blank(x)) return(c(NA, NA) |> as.integer())
    if(length(x) == 1) return(c(x, NA) |> as.integer())
    if(!is.call(x) || x[[1]] != ":") stop("problem with some indices")
    c(x[[2]], x[[3]]) |> as.integer()
  }
  do.call("rbind",
          lapply(indices, extractRow)) %||%
  matrix(0L, nrow = 0, ncol = 2) # 0L makes it integer even though empty
}

determineNdimFromOneCase <- function(model, varAndIndices) {
    #varInfo <- try(model$getVarInfo(as.character(varAndIndices$varName)))
    varInfo <- try(model$modelDef$varInfo[[as.character(varAndIndices$varName)]])
    if(inherits(varInfo, 'try-error')) stop(paste0('In determineNdimFromOneCase: error in extracting varInfo for ', varAndIndices$varName), call. = FALSE)
    varNdim <- varInfo$nDim
    if(length(varAndIndices$indices) == 0) return(varNdim)
    if(length(varAndIndices$indices) != varNdim) {
        stop(paste0('Error, wrong number of dimensions in a node label for ', varAndIndices$varName, '.  Expected ',varNdim,' indices but got ', length(varAndIndices$indices),'.'))
    }
    dropNdim <- sum(unlist(lapply(varAndIndices$indices, is.numeric)))
    return(varNdim - dropNdim)
}

determineNdimsFromNfproc <- function(modelExpr, varOrNodeExpr, nfProc) {
    allNDims <- lapply(nfProc$instances, function(x) {
        model <- eval(modelExpr, envir = x)
        if(length(varOrNodeExpr) > 1)
            stop("One must request a node from a model using syntax like `model[[node]]` and not syntax such as `model[[nodes[i]]]`. For the latter case use `values()` instead.")
        if(!exists(as.character(varOrNodeExpr), x, inherits = FALSE) ) {
            stop(paste0('Problem accessing node or variable ', deparse(varOrNodeExpr), '.'), call. = FALSE)
        }
        lab <- eval(varOrNodeExpr, envir = x)
        if(length(lab) != 1)
            stop(paste0("Length of ",
                        deparse(varOrNodeExpr),
                        " requested from ",
                        deparse(modelExpr),
                        " using '[[' is ",
                        length(lab),
                        ". It must be 1." )
               , call. = FALSE)
        varAndIndices <- getVarAndIndices(lab)
        determineNdimFromOneCase(model, varAndIndices)
    } )
    return(allNDims)
}

#' @export
setNimType <- function(x, value) {
  attr(x, "nimble_type") <- value
  x
}
