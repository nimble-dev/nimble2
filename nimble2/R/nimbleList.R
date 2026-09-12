# In nimble2, we implement nimbleList using nClass.
# We don't need to use nClassBuilder because nimbleList types
# (i.e. with specific fields) are not created dynamically within 
# run code, such as for nCompiler::nList.

nimbleListBase_nClass <- nCompiler:::nClass(
  classname = "nimbleList",
  Cpublic = list(dummy_ = "numericScalar()"), # To-Do: fix nClass to allow total emptyness. For now provide one field.
  Rpublic = list(
    initialize = function(...) {
      super$initialize(...)
      self$dummy_ <- 0
    })
)

# nimbleList <- function(...) {
#   # return nClass generator.
#   # - nimble requires nested definitions be findable without lazyness,
#   # so the types here can be resolved already if needed.
#   # - Inherit from nimbleListBase_nClass for the class tag for various steps
#   # where we need to check for nl objects
#   # - We'll add NCgenerator as a field similarly to nimbleModel
#   #.  where it is set as a field after the generator is created by R6Class.

# }

make_nimbleList_hashID <- function(types) {
  # We go to some lengths to normalize
  # so that we obtain the same hash for equivalent varInfo.
  #
  # to-do: handle nCompiler more general type declarations.
  varNames <- types$vars
  sortOrder <- order(varNames)
  types$vars <- types$vars[sortOrder]
  types$types <- types$types[sortOrder]
  types$dims <- types$dims[sortOrder]
  types <- types[c("vars", "types", "dims")]
  hashedID <- digest::digest(types, algo = "crc32")
  hashedID
}

#' @export
nimbleList <- function(..., predefined = FALSE, .ID = FALSE, env = parent.frame(), where = parent.frame()) {
  # to-do: support name, predefined, and where.
  # .ID and env are standard arguments for an nClassBuilder.
  message("must deal with env, where and other nimbleList arguments")

  # Start of code copied from nimble::nimbleList
    ## This manual override allows us to generate static code by temporarily setting
  ## predefined = FALSE for all predefined nimbleLists.
  GENERATE_STATIC_CODE <- FALSE  ## Enable this before using generateStaticCode.R.
  if(GENERATE_STATIC_CODE) predefined <- FALSE
  
  ## 3 possibilities: arguments as expressions, arguments as list created within call,
  ## arguments as list created outside of call
  
  Call <-  match.call(expand.dots = TRUE)
  nms <- names(Call)
  if(any(nms == 'name')) {
    if(!is.character(Call[[which(nms == "name")]]))
      stop("Elements of a nimbleList cannot be named `name`.")
    Call <- Call[-which(names(Call) == 'name')]
  }
  nms <- names(Call)
  if(any(nms == 'predefined')) {
    if(!is.logical(Call[[which(nms == "predefined")]]))
      stop("Elements of a nimbleList cannot be named `predefined`.")
    Call <- Call[-which(nms == 'predefined')]
  }
  nms <- names(Call)
  if(any(nms == 'where')) {
    if(!is.environment(Call[[which(nms == "where")]]))
      stop("Elements of a nimbleList cannot be named `where`.")
    Call <- Call[-which(nms == 'where')]
  }
  if(length(Call) < 2)
    stop("No arguments specified for nimbleList")
  argList <- list()
  
  ## left side of || statement catches list(...) arguments
  ## right side captures name of a previously defined list
  if((is.call(Call[[2]]) && deparse(Call[[2]][[1]]) == 'list') || 
     (!is.call(Call[[2]]) && is.list(eval(Call[[2]], envir = parent.frame())))){ 
    callList <- eval(Call[[2]], envir = parent.frame())
    for(iArg in seq_along(callList)){
      argList[[iArg]] <- list(name = callList[[iArg]]$name,
                              type = callList[[iArg]]$type,
                              dim = callList[[iArg]]$dim)
    }
  }
  else{  ## if arguments are expressions e.g. nimListDouble = double(2)
    for(iArg in 2:length(Call)){
      argList[[iArg-1]] <- list(name = names(Call)[iArg],
                                type = deparse(Call[[iArg]][[1]]))
      argList[[iArg-1]]$dim  <- if(length(Call[[iArg]])>1) deparse(Call[[iArg]][[2]])
      else 0
    }
  }
  
  types <- list(vars = sapply(argList, function(x){return(x$name)}),
                types =  sapply(argList, function(x){return(x$type)}),
                dims =  sapply(argList, function(x){return(x$dim)}))
  
  if(any(c('name','predefined','where') %in% types$vars))
    stop("Elements of a nimbleList cannot be named `name`, `predefined` or `where`.")
  # End of code copied directly from nimble::nimbleList

  hashedID <- make_nimbleList_hashID(types)
  cpp_classname <- Rname2CppName(paste0("NL_", hashedID))
  if (isTRUE(.ID)) {
    return(cpp_classname)
  }
  ans <- make_nimbleList_nClass(types, 
                                name = cpp_classname,
                                predefined = predefined,
                                where = where)
  ans
}
class(nimbleList) <- c("function", "nClassBuilder")

nimbleListDefClass <- setRefClass(
    ## This class holds a list of type information such as
    ## A = double(1), B = integer(2)
    ## The types need not be numeric.
    ## In general, ideally, they could be another nimbleList or a nimbleFunction
    Class = "nimbleListDefClass",
    fields = list(types = 'ANY',
                  className = 'ANY',
                  predefined = 'ANY')
)

make_nimbleList_nClass <- function(types,
                                   name = NULL,
                                   predefined = predefined,
                                   where =  parent.frame()) {

  if(is.na(name)) name <- nf_refClassLabelMaker()
  nlDefClassObject <- nimbleListDefClass(types = types, className = name, predefined = predefined) 
  basicTypes <- c("double", "integer", "character", "logical")
  nestedListGens <- list()
  for(i in seq_along(types$types)){
    if(!(types$types[i] %in% basicTypes)){
      for(searchEnvironment in c(parent.frame(), globalenv())){
        ## It could become necessary to add "asNamespace("nimble")" to the searchEnvironment list
        found_nlGen <- try(get(types$types[i], envir = searchEnvironment), silent = TRUE)
        if(!inherits(found_nlGen, 'try-error')){
          if(is.nlGenerator(found_nlGen))
            nestedListGens[[types$vars[i]]] <- found_nlGen
          break
        }
      }
    }
  }
  
  # Above code is all copied from nimble.
  # From here down it is used for nimble2 and nCompiler.
  # It could be written anew and made cleaner, but this provides
  # a simple path from nimble to nimble2
  
  # This initialize is copied and lightly modified from nimble
  initialize <- function(...){
    super$initialize(...)
    if(!isCompiled()) {
      NLdef <- self$nimbleListDef
      nimListFields <- NLdef$types$vars
      initializeFields <- list(...)
      nonInitializeFields <- which(!(nimListFields %in% names(initializeFields)))
      ## initialize uninitialized fields
      for(i in nonInitializeFields){
        thisType <- NLdef$types$types[i]
        thisDim <-  NLdef$types$dims[i]
        if(thisType == 'character'){
          initValue  <- ""
        }
        else if(thisType %in% c('integer', 'double')){
          if(thisDim == 0)
            initValue <- 0
          if(thisDim == 1)
            initValue <- integer(0)
          if(thisDim == 2)
            initValue <- matrix(0, 0, 0)
          if(thisDim > 2)
            initValue <- array(0, dim = rep(0, thisDim))
        }
        else if(thisType == 'logical'){
          initValue <- FALSE
        }
        else if(nimListFields[i] %in% names(nestedListGenList)){
          initValue <- nestedListGenList[[nimListFields[i]]]$new()
        }
        else(stop(paste("unrecognized type given for nimbleList element", nimListFields[i])))
        eval(substitute(self[[nimListFields[i]]] <<-initValue))
      }
    } 
  }
  CpublicVars <- paste0("numericArray(nDim=", types$dims, ")") |> as.list()
  names(CpublicVars) <- types$vars
  
  classname <- name
  
  generator_code <- substitute(
    nCompiler::nClass(
      classname = CLASSNAME,
      inherit = nimble2:::nimbleListBase_nClass,
      compileInfo = list(),
      Rpublic = RPUBLIC,
      Cpublic = CPUBLIC,
      env = where
    ),
    list(
      RPUBLIC = list(
        nimbleListDef = nlDefClassObject,
        nestedListGenList = nestedListGens,
        initialize = initialize
      ),
      # A concatenation of lists
      CPUBLIC = CpublicVars,
      CLASSNAME = classname
    )
  )
  nl_generator <- eval(generator_code)
  nl_generator$set("public", "NCgenerator", nl_generator)
  where$nl_generator <- nl_generator
  nl_generator
}

## helper function for creating argument lists
nf_createAList <- function(argNames) {
  aListText <- if(length(argNames) > 0) {
    argNames <- paste0('`', argNames, '`')
    paste0('alist(', paste(argNames,'=',collapse=','), ')')
  } else { 'alist()' }
  eval(parse(text = aListText, keep.source = FALSE))
}

# Used by keyword processing:
makeNimbleListTemplateWithBlankFirstArg <- function(nlDef) {
  vars <- c('.LEFTSIDE', nlDef$types$vars)
  functionAsList <- list(as.name('function'))
  functionAsList[2] <- list(NULL)
  if(length(vars) > 0) {
      argsList <- nf_createAList(vars)
      functionAsList[[2]] <- as.pairlist(argsList)
  }
  functionAsList[[3]] <- quote({})
  eval(as.call(functionAsList))
}

nl.getListDef <- function(nlGen) {
  nlGen$public_fields$nimbleListDef
}