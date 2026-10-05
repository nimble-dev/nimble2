#' Access or set a member variable of a nimbleFunction
#'
#' Internal way to access or set a member variable of a nimbleFunction created during \code{setup}.  Normally in NIMBLE code you would use \code{nf$var} instead of \code{nfVar(nf, var)}.
#'
#' @aliases nfVar nfVar<-
#'
#' @description
#' Access or set a member variable of a specialized nimbleFunction, i.e. a variable passed to or created during the \code{setup} function that is used in run code or preserved by \code{setupOutputs}.  Works in R for any variable and in NIMBLE for numeric variables.
#'
#' @param nf      a specialized nimbleFunction, i.e. a function returned by executing a function returned from \code{nimbleFunction} with \code{setup} arguments
#' @param varName a character string naming a variable in the \code{setup} function.
#' @param value value to set the variable to.
#' @author NIMBLE development team
#' @export
#' @details
#' When \code{nimbleFunction} is called and a \code{setup} function is provided, then \code{nimbleFunction} returns a function.  That function is a generator that should be called with arguments to the \code{setup} function and returns another function with \code{run} and possibly other member functions.  The member functions can use objects created or passed to \code{setup}.  During internal processing, the NIMBLE compiler turns some cases of \code{nf$var} into \code{nfVar(nf, var)}. These provide direct access to setup variables (member data).  \code{nfVar} is not typically called by a NIMBLE user or programmer.
#'
#'
#' For internal access to methods of \code{nf}, see \code{\link{nfMethod}}.
#'
#' For more information, see \code{?nimbleFunction} and the NIMBLE \href{https://r-nimble.org/manual/cha-welcome-nimble.html}{User Manual}.
#'
#' @return whatever varName is in the nimbleFunction nf.
#' @examples
#' nfGen1 <- nimbleFunction(
#'   setup = function(A) {
#'     B <- matrix(rnorm(4), nrow = 2)
#'     setupOutputs(B) ## preserves B even though it is not used in run-code
#'   },
#'   run = function() {
#'     print("This is A", A, "\n")
#'   }
#' )
#'
#' nfGen2 <- nimbleFunction(
#'   setup = function() {
#'     nf1 <- nfGen1(1000)
#'   },
#'   run = function() {
#'     print("accessing A:", nfVar(nf1, "A"))
#'     nfVar(nf1, "B")[2, 2] <<- -1000
#'     print("accessing B:", nfVar(nf1, "B"))
#'   }
#' )
#'
#' nf2 <- nfGen2()
#' nf2$run()
nfVar <- function(nf, varName) {
  refClassObj <- nf_getRefClassObject(nf)
  v <- refClassObj[[varName]]
  return(v)
}

#' @rdname nfVar
#' @export
`nfVar<-` <- function(nf, varName, value) {
  refClassObj <- nf_getRefClassObject(nf)
  refClassObj[[varName]] <- value
  return(nf)
}

#' Creates numeric, integer or logical vectors for use in nimbleFunctions
#'
#' In a \code{nimbleFunction}, \code{numeric}, \code{integer} and \code{logical} are identical to \code{nimNumeric}, \code{nimInteger} and \code{nimLogical}, respectively.
#'
#' @aliases nimInteger nimLogical numeric integer logical
#'
#' @param length the length of the vector (default = 0)
#' @param value value(s) for initializing the vector (default = 0).  This may be a vector, matrix or array but will be used as a vector.
#' @param init logical, whether to initialize elements of the vector (default = TRUE)
#' @param fillZeros logical, whether to initialize any elements not filled by (possibly recycled) \code{value} with 0 (or FALSE for \code{nimLogical}) (default = TRUE)
#' @param recycle logical, whether \code{value} should be recycled to fill the entire \code{length} of the new vector (default = TRUE)
#'
#' @details
#' These functions are similar to R's \code{\link{numeric}}, \code{\link{integer}}, \code{\link{logical}} functions, but they can be used in a nimbleFunction and then compiled using \code{compileNimble}.  Largely for compilation purposes, finer control is provided over initialization behavior.  If \code{init = FALSE}, no initialization will be done, and \code{value}, \code{fillZeros} and \code{recycle} will be ignored.  If \code{init=TRUE} and \code{recycle=TRUE}, then \code{fillZeros} will be ignored, and \code{value} will be repeated (according to R's recycling rule) as much as necessary to fill a vector of length \code{length}.  If \code{init=TRUE} and \code{recycle=FALSE}, then if \code{fillZeros=TRUE}, values of 0 (or FALSE for \code{nimLogical}) will be filled in after \code{value} up to length \code{length}.  Compiled code will be more efficient if unnecessary initialization is not done, but this may or may not be noticeable depending on the situation.
#' 
#' When used in a \code{nimbleFunction} (in \code{run} or other member function), \code{numeric}, \code{integer} and \code{logical} are immediately converted to \code{nimNumeric}, \code{nimInteger} and \code{nimLogical}, respectively.  
#' 
#' @author Daniel Turek, Christopher Paciorek, Perry de Valpine
#' @aliases numeric
#' @seealso \code{\link{nimMatrix}}, \code{\link{nimArray}}
#' @export
nimNumeric <- function(length = 0, value = 0, init = TRUE, fillZeros = TRUE, recycle = TRUE) {
    fillValue <- makeFillValue(value, 'double', init)
    makeReturnVector(fillValue, length, recycle)
}

#' @rdname nimNumeric
#' @export
nimInteger <- function(length = 0, value = 0, init = TRUE, fillZeros = TRUE, recycle = TRUE) {
    fillValue <- makeFillValue(value, 'integer', init)
    makeReturnVector(fillValue, length, recycle)
}

#' @rdname nimNumeric
#' @export
nimLogical <- function(length = 0, value = 0, init = TRUE, fillZeros = TRUE, recycle = TRUE) {
    fillValue <- makeFillValue(value, 'logical', init)
    makeReturnVector(fillValue, length, recycle)
}

#' Creates matrix or array objects for use in nimbleFunctions
#' 
#' In a \code{nimbleFunction}, \code{matrix} and \code{array} are identical to \code{nimMatrix} and \code{nimArray}, respectively
#'
#' @aliases nimArray matrix array
#'
#' @param value value(s) for initialization (default = 0).  This can be a vector, matrix or array, but it will be used as a vector.
#' @param nrow the number of rows in a matrix (default = 1)
#' @param ncol the number of columns in a matrix (default = 1)
#' @param dim vector of dimension sizes in an array (default = \code{c(1, 1)})
#' @param init logical, whether to initialize values (default = \code{TRUE})
#' @param fillZeros logical, whether to initialize any elements not filled by (possibly recycled) \code{value} with 0 (or \code{FALSE} for \code{nimLogical}) (default = \code{TRUE})
#' @param recycle logical, whether \code{value} should be recycled to fill the entire contents of the new object (default = \code{TRUE})
#' @param type character representing the data type, i.e. \code{'double'}, \code{'integer'}, or \code{'logical'} (default = \code{'double'})
#' @param nDim number of dimensions in an array.  This is only necessary for \code{compileNimble} if the length of \code{dim} cannot be determined during compilation.
#'
#' @details
#' These functions are similar to R's \code{\link{matrix}} and \code{\link{array}} functions, but they can be used in a nimbleFunction and compiled using \code{compileNimble}.  Largely for compilation purposes, finer control is provided over initialization behavior, similarly to \code{\link{nimNumeric}}, \code{\link{nimInteger}}, and \code{\link{nimLogical}}. If \code{init = FALSE}, no initialization will be done, and \code{value}, \code{fillZeros} and \code{recycle} will be ignored.  If \code{init=TRUE} and \code{recycle=TRUE}, then \code{fillZeros} will be ignored, and \code{value} will be repeated (according to R's recycling rule) as much as necessary to fill the object.  If \code{init=TRUE} and \code{recycle=FALSE}, then if \code{fillZeros=TRUE}, values of 0 (or FALSE for \code{nimLogical}) will be filled in after \code{value}.  Compiled code will be more efficient if unnecessary initialization is not done, but this may or may not be noticeable depending on the situation.
#'
#' When used in a \code{nimbleFunction} (in \code{run} or other member function), \code{matrix} and \code{array} are immediately converted to \code{nimMatrix} and \code{nimArray}, respectively.
#'
#' The \code{nDim} argument is only necessary for a use like \code{dim <- c(2, 3, 4); A <- nimArray(0, dim = dim, nDim = 3)}.  It is necessary because the NIMBLE compiler must determine during compilation that \code{A} will be a 3-dimensional numeric array.  However, the compiler doesn't know for sure what the length of \code{dim} will be at run time, only that it is a vector.  On the other hand,   \code{A <- nimArray(0, dim = c(2, 3, 4))} is allowed because the compiler can directly determine that a vector of length three is constructed inline for the \code{dim} argument.
#' 
#' @author Daniel Turek and Perry de Valpine
#' @seealso \code{\link{nimNumeric}} \code{\link{nimInteger}} \code{\link{nimLogical}}
#' @export
nimMatrix <- function(value = 0, nrow = NA, ncol = NA, init = TRUE, fillZeros = TRUE, recycle = TRUE, type = 'double') {
    ## the -1's are used because nimble does not allow both missingness and default value
    ## but R's matrix function relies on both possibilities
    fillValue <- makeFillValue(value, type, init)
    mnrow <- missing(nrow) || is.na(nrow)
    mncol <- missing(ncol) || is.na(ncol)
    if(mnrow)
        if(mncol) {
            base::matrix(fillValue)
        } else {
            nrow <- ceiling( length(fillValue) / ncol )
            fillValue <- makeReturnVector(fillValue, nrow * ncol, recycle)
            base::matrix(fillValue, ncol = ncol, nrow = nrow)
        }
    else
        if(mncol) {
            ncol <- ceiling( length(fillValue) / nrow )
            fillValue <- makeReturnVector(fillValue, nrow * ncol, recycle)
            base::matrix(fillValue, nrow = nrow, ncol = ncol)
        } else {
            fillValue <- makeReturnVector(fillValue, ncol*nrow, recycle)
            base::matrix(fillValue, nrow = nrow, ncol = ncol)
        }
}


#' @rdname nimMatrix
#' @export
nimArray <- function(value = 0, dim = c(1, 1), init = TRUE, fillZeros = TRUE, recycle = TRUE, nDim, type = 'double') {
    if(!missing(nDim)) dim <- dim[1:nDim]
    fillValue <- makeFillValue(value, type, init)
    fillValue <- makeReturnVector(fillValue, prod(dim), recycle)
    if(length(dim) == 1) fillValue
    else base::array(fillValue, dim)
}

makeFillValue <- function(value, type, init) {
    fillValue <- if(init) value else 0
    fillValueTyped <- switch(type,
                             double = as.numeric(fillValue),
                             integer = as.integer(fillValue),
                             logical = as.logical(fillValue),
                             stop('unknown type argument'))
    return(fillValueTyped)
}

makeReturnVector <- function(fillValue, length, recycle) {
    if(length(fillValue) == 1) {
        if(recycle)
            rep(fillValue, length)
        else
            c(fillValue, as(rep(0, max(length-1, 0)), class(fillValue)))
    }
    else {
        if(length(fillValue) != length) {
            if(length(fillValue) < length) {
                ##warning(paste0("Not enough values provided for vector of length ",length, ".")) 
                if(recycle)
                    rep(fillValue, length.out = length)
                else
                    c(fillValue, as(rep(0, length-length(fillValue)), class(fillValue)))
            } else {
                ##warning(paste0("Too many values provided for vector of length ",length, ".")) 
                fillValue[1:length]
            }
        } else {
            fillValue
        }
    }
}

#' NIMBLE language functions for R-like vector construction
#'
#' The functions \code{c}, \code{rep}, \code{seq}, \code{which}, \code{diag}, \code{length}, \code{seq_along}, \code{is.na}, \code{is.nan}, \code{any}, and \code{all} can be used in nimbleFunctions and compiled using \code{compileNimble}.
#'
#' @name nimble-R-functions
#'
#' @param ... values to be concatenated.
#' @param x vector of values to be replicated (\code{rep}), or logical array or vector (\code{which}), or object whose length is wanted (\code{length}), or input value (\code{diag}), or vector of values to be tested/checked (\code{is.na}, \code{is.nan}, \code{any}, \code{all}).
#' @param from starting value of sequence.
#' @param to end value of sequence.
#' @param by increment of the sequence.
#' @param length.out desired length of the sequence.
#'
#' @aliases nimC nimRep nimSeq c rep seq which diag length seq_along is.na is.nan any all
#'
#' @details
#' For \code{c}, \code{rep}, \code{seq}, these functions are NIMBLE's version of similar R functions, e.g., \code{nimRep} for \code{rep}.   In a \code{nimbleFunction}, either the R name (e.g., \code{rep}) or the NIMBLE name (e.g., \code{nimRep}) can be used.  If the R name is used, it will be converted to the NIMBLE name. For \code{which}, \code{length}, \code{diag}, \code{seq_along}, \code{is.na}, \code{is.nan}, \code{any}, \code{all} simply use the standard name without \code{"nim"}. These functions largely mimic (see exceptions below) the behavior of their R counterparts, but they can be compiled in a \code{nimbleFunction} using \code{compileNimble}.
#' 
#' \code{nimC} is NIMBLE's version of \code{c} and behaves identically.
#'
#' \code{nimRep} is NIMBLE's version of \code{rep}.  It should behave identically to \code{rep}.  There are no NIMBLE versions of \code{rep.int} or \code{rep_len}.
#'
#' \code{nimSeq} is NIMBLE's version of \code{seq}.  It behaves like \code{seq} with support for \code{from}, \code{to}, \code{by} and \code{length.out} arguments.  The \code{along.with} argument is not supported.  There are no NIMBLE versions of \code{seq.int}, \code{seq_along} or \code{seq_len}, with the exception that \code{seq_along} can take a nimbleFunctionList as an argument to provide the index range of a for-loop (\href{https://r-nimble.org/manual/cha-welcome-nimble.html}{User Manual} Ch. 13). 
#'
#' \code{which} behaves like the R version but without support for \code{arr.ind} or \code{useNames} arguments.
#'
#' \code{diag} behaves like the R version but without support for the \code{nrow} and \code{ncol} arguments.
#'
#' \code{length} behaves like the R version.
#' 
#' \code{seq_along} behaves like the R version.
#'
#' \code{is.na} behaves like the R version but does not correctly handle \code{NA} values from R that are type 'logical', so convert these using \code{as.numeric()} before passing from R to NIMBLE.
#' 
#' \code{is.nan} behaves like the R version, but treats \code{NA} of type 'double' as being \code{NaN} and \code{NA} of type 'logical' as not being \code{NaN}. 
#' 
#' \code{any} behaves like the R version but takes only one argument and treats NAs as \code{FALSE}.
#'
#' \code{all} behaves like the R version but takes only one argument and treats NAs as \code{FALSE}.
#'
NULL

#' @rdname nimble-R-functions
#' @export
nimC <- function(...) {
    c(...)
}
