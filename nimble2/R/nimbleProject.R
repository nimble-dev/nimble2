# We need at least enough of the nimbleProject
# structure here in nimble2 to track recursive finding
# of setup output objects during keyword processing.
# We will use an R6 class instead of a reference class.
#
# The *compilationClasses can be replaced by simple lists

#' @importFrom rlang env_label
nimbleProjectClass <- R6::R6Class(
  classname = "nimbleProjectClass",
  portable = FALSE,
  public = list(
    # RCfunInfos         =  'ANY',		#'list', ## a list of RCfunInfoClass objects
    # RCfunCppInterfaces =  'ANY',		#'list',
    RCfuns = list(),
    # mvInfos            =  'ANY',		#'list', ## a list of mvInfoClass objects
    # modelDefInfos      =  'ANY',		#'list',
    # models             =  'ANY',		#'list',
    NFgens = list(),
    # nimbleLists        =  'ANY',   #'list',
    # nfCompInfos        =  'ANY',		#'list', ## list of nfCompilationInfoClass objects
    # nlCompInfos        =  'ANY',   #'list', ## list of nfCompilationInfoClass objects
    # cppProjects        =  'ANY',		#'list', ## list of cppProjectClass objects, 1 for each dll to be produced
    # dirName            =  'ANY',		#'character',
    # nimbleLabel        =  'ANY',		#'character',
    # refClassDefsEnv    =  'ANY',		#'environment',
    projectName = "ANY", #' character'
    print = function() {
      writeLines(paste0("nimbleProject object"))
    },
    initialize = function(name = "") {
      if (name == "") {
        projectName <<- projectNameCreator()
      } else {
        projectName <<- name
      }
    },

    # Refactor the steps for an RCfunction (nimbleFunction with no setup code)
    RCfunction_add = function(obj, control = list(), ...) {
      if (!is.rcf(obj)) stop("Argument to RCfunction_add is not an RCfunction.", call. = FALSE)
      obj_label <- nCompiler::NFinternals(obj)$uniqueName
      if (is.null(RCfuns[[obj_label]])) {
        RCfuns[[obj_label]] <<- obj
      }
      obj
    },
    RCfunction_need = function(obj) {
      RCfunction_add(obj)
      obj
    },
    # Refactor the steps to add a set of nimbleFunction objects (with setup code)
    # This replaces compiledNimbleFunctionMulti.
    nimbleFunction_add_multi = function(funList,
                                        control = list(),
                                        generatorFunNames = NULL) {
      if (!is.list(funList)) {
        stop("funList in nimbleFunction_add_multi should be a list", call. = FALSE)
      }
      allGeneratorNames <-
        if (is.null(generatorFunNames)) {
          lapply(funList, nfGetDefVar, "name")
        } else {
          generatorFunNames
        }
      uniqueGeneratorNames <- unique(allGeneratorNames)
      # I am not sure ans is needed here.
      ans <- vector('list', length(funList))
      for (uGN in uniqueGeneratorNames) {
        thisBool <- allGeneratorNames == uGN
        thisAns <- nimbleFunction_add(funList[thisBool],
          control = control,
          generatorName = uGN
        )
        ans[thisBool] <- NFgens[uGN]
      }
      ans
    },

    # Refactor the steps for a nimbleFunction (which means with setup code)
    nimbleFunction_track = function(obj, generatorName = NULL) {
      if (is.null(generatorName)) {
        generatorName <- nfGetDefVar(obj, "name")
      }
      if (is.null(NFgens[[generatorName]])) {
        ## nfProc could have been created already during makeTypeObject for another nimbleFunction so it knows the types of this one.
        NFgens[[generatorName]] <<-
          list(
            nfGenerator = nf_getGeneratorFunction(obj),
            RinitTypesProcessed = FALSE,
            instances = list(),
            nfProc = NULL
          )
      }
      instances <- NFgens[[generatorName]]$instances
      obj_label <- rlang::env_label(as.environment(obj))
      if (is.null(instances[[obj_label]])) {
        NFgens[[generatorName]]$instances[[obj_label]] <<- obj
      }
      obj
    },
    # nimbleFunction_setup_proc replaces buildNimbleFunctionCompilationInfo.
    nimbleFunction_setup_proc = function(generatorName) {
      if(!is.null(NFgens[[generatorName]]$nfProc)) {
        return(NFgens[[generatorName]]$nfProc)
      }
      if (!length(NFgens[[generatorName]]$instances)) {
        stop("Requested nimbleFunction_setup_proc for a generator with no instances.", call. = FALSE)
      }
      NFgens[[generatorName]]$nfProc <<- 
        nfProcessing(NFgens[[generatorName]]$instances, generatorName, project = self)
      NFgens[[generatorName]]$nfProc
    },
    # nimbleFunction_add replaces compileNimbleFunction with initialTypeInference=TRUE
    nimbleFunction_add = function(fun, generatorName = NULL, control=list(), ...) {
      # reset argument has been removed and may be re-added if necessary.
      # fun could be character (a generator name) or a singleton or a list
      if (is.character(fun)) {
        # could be the name of a generator.
        tmp <- NFgens[[fun]]
        if (is.null(tmp)) stop(paste0("nimbleFunction generator name ", fun, " not recognized in this project."), call. = FALSE)
        fun <- tmp
        funList <- list(fun)
        generatorName <- nfGetDefVar(fun, "name")
      } else {
        if (is.list(fun)) {
          if (length(fun) == 0) stop("Empty list provided to nimbleFunction_initialize", call. = FALSE)
          # generatorName would have been provided from the multi case
          if (is.null(generatorName)) {
            generatorName <- unique(unlist(lapply(fun, nfGetDefVar, "name")))
          }
          if (length(generatorName) != 1) {
            stop(paste0(
              "Not all objects provided to nimbleFunction_initialize are from the same nimbleFunction.",
              " The nimbleFunction generator names include:", paste(generatorName, collapse = " ")
            ), call. = FALSE)
          }
          funList <- fun
        } else {
          if (!is.nf(fun)) stop(paste0("fun argument to nimbleFunction_initialize is not a nimbleFunction."), call. = FALSE)
          funList <- list(fun)
          generatorName <- nfGetDefVar(fun, "name")
        }
        # The alreadyAdded logical has been removed.
        # instances <- NFgens[[generatorName]]$instances
        for (i in seq_along(funList)) {
          addNF <- TRUE # previously there was more involved logic. This is a reminder.
          if (addNF) {
            nimbleFunction_track(funList[[i]], generatorName = generatorName)
          }
        }
      }
      nfProc <- nimbleFunction_setup_proc(generatorName = generatorName)
      nfProc
    },
    process = function() {
      for(i in seq_along(NFgens)) {
        nfProc <- nimbleFunction_setup_proc(generatorName = names(NFgens)[i])
        nfProc$process()
      }
    },
    get_nComp_units = function() {
      nClass_units <- NFgens |> lapply(\(x) x$nfProc$nClassGen) |> setNames(names(NFgens))
      c(RCfuns, nClass_units)
    },
    instantiate = function(units, comp) {
      # instantiate nClass objects
      browser()
      compiled_units <- vector("list", length = length(units))
      for(i in seq_along(units)) {
        generatorName <- nfGetDefVar(units[[i]], "name")
        nCgen <- comp[[generatorName]]
        obj <- nCgen$new()
        # The setupOutputNames will need to be expanded when there is new setup code or inheritance.
        # This is a first step
        setupOutputNames <- nf_getSetupOutputNames(NFgens[[generatorName]]$nfGenerator)
        setupOutputList <- setupOutputNames |> lapply(\(x) units[[i]][[x]]) |> setNames(setupOutputNames)
        nCompiler::value(obj) <- setupOutputList
        compiled_units[[i]] <- obj
      }
      compiled_units
    }
  )
)
