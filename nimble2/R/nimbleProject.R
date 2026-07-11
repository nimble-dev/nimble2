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
    modelGens = list(),
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
    #################
    ## RCfunctions ##
    #################
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
    ############
    ## models ##
    ############
    model_add = function(obj, control = list(), ...) {
      # A model will be an nClass object.
      if (!inherits(obj, "modelBase_nClass")) {
        stop("Argument to model_add is not a nimble model", call. = FALSE)
      }
      NCgen <- obj$NCgenerator # models hold this (nClass objects generally do not hold their generator)
      NCgen_label <- nCompiler::NCinternals(NCgen)$classID
      if (is.null(modelGens[[NCgen_label]])) {
        modelGens[[NCgen_label]] <<-
          list(
            NCgenerator = NCgen,
            instances = list(),
            compiled_instances = list()
          )
      }
      instances <- modelGens[[NCgen_label]]$instances
      obj_label <- rlang::env_label(as.environment(obj))
      if (is.null(instances[[obj_label]])) {
        modelGens[[NCgen_label]]$instances[[obj_label]] <<- obj
      }
      obj
    },
    model_getResults = function(obj) {
      NCgen <- obj$NCgenerator
      NCgen_label <- nCompiler::NCinternals(NCgen)$classID
      if (is.null(modelGens[[NCgen_label]])) {
        stop("model generator not found in project", call. = FALSE)
      }
      obj_label <- rlang::env_label(as.environment(obj))
      compiled_instances <- modelGens[[NCgen_label]]$compiled_instances
      if (is.null(compiled_instances[[obj_label]])) {
        stop("compiled model instance not found in project", call. = FALSE)
      }
      compiled_instances[[obj_label]]
    },
    model_get_compiled_internal = function(obj) {
      # This is called when instantiating a nimbleFunction that may need the compiled model object.
      NCgen <- obj$NCgenerator
      NCgen_label <- nCompiler::NCinternals(NCgen)$classID
      if (is.null(modelGens[[NCgen_label]])) {
        stop("model generator not found in project", call. = FALSE)
      }
      compiled_instances <- modelGens[[NCgen_label]]$compiled_instances
      obj_label <- rlang::env_label(as.environment(obj))
      if (is.null(compiled_instances[[obj_label]])) {
        stop("compiled model instance not found in project", call. = FALSE)
      }
      compiled_instances[[obj_label]]
    },
    model_instantiate = function(genName, compiled_generator) {
      instances <- modelGens[[genName]]$instances
      if (!length(instances)) {
        return(invisible(NULL))
      }
      compiled_instances <-
        seq_along(instances) |>
        lapply(\(x) compiled_generator$new()) |>
        setNames(names(instances))
      modelGens[[genName]]$compiled_instances <<- compiled_instances
    },
    model_populate = function(genName) {
      instances <- modelGens[[genName]]$instances
      if (!length(instances)) {
        return()
      }
      compiled_instances <- modelGens[[genName]]$compiled_instances
      varNames <- c(
        instances[[1]]$modelDef$varInfo |> names(),
        instances[[1]]$modelDef$logProbVarInfo |> names()
      )
      for (i in seq_along(instances)) {
        initList <- lapply(varNames, \(x) instances[[i]][[x]]) |> setNames(varNames)
        initList <- initList[lapply(initList, is.numeric) |> unlist()]
        nCompiler::value(compiled_instances[[i]]) <- initList
      }
    },
    #####################
    ## nimbleFunctions ##
    #####################
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
      ans <- vector("list", length(funList))
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
    nimbleFunction_getResults = function(units) {
      compiled_units <- vector("list", length(units))
      names(compiled_units) <- names(units)
      allGeneratorNames <-
        lapply(units, nfGetDefVar, "name")
      uniqueGeneratorNames <- unique(allGeneratorNames)
      for (uGN in uniqueGeneratorNames) {
        if (!uGN %in% names(NFgens)) {
          stop(paste0("nimbleFunction generator ", uGN, " not found in project"), call. = FALSE)
        }
        thisBool <- allGeneratorNames == uGN
        env_labels <- units[thisBool] |>
          lapply(\(x) rlang::env_label(as.environment(x))) |>
          unlist()
        compiled_instances <- NFgens[[uGN]]$compiled_instances
        if (!all(env_labels %in% names(compiled_instances))) {
          stop(paste0("Not all instances of nimbleFunction generator ", uGN, " have been compiled."), call. = FALSE)
        }
        compiled_units[thisBool] <- compiled_instances[env_labels]
      }
      compiled_units
    },
    # nimbleFunction_add replaces compileNimbleFunction with initialTypeInference=TRUE
    nimbleFunction_add = function(fun, generatorName = NULL, control = list(), ...) {
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
            compiled_instances = list(),
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
      if (!is.null(NFgens[[generatorName]]$nfProc)) {
        return(NFgens[[generatorName]]$nfProc)
      }
      if (!length(NFgens[[generatorName]]$instances)) {
        stop("Requested nimbleFunction_setup_proc for a generator with no instances.", call. = FALSE)
      }
      NFgens[[generatorName]]$nfProc <<-
        nfProcessing(NFgens[[generatorName]]$instances, generatorName, project = self)
      NFgens[[generatorName]]$nfProc
    },
    nimbleFunction_instantiate = function(generatorName, compiled_generator) {
      instances <- NFgens[[generatorName]]$instances
      if (!length(instances)) {
        return(invisible(NULL))
      }
      compiled_instances <-
        seq_along(instances) |>
        lapply(\(x) compiled_generator$new()) |>
        setNames(names(instances))
      NFgens[[generatorName]]$compiled_instances <<- compiled_instances
    },
    nimbleFunction_populate = function(generatorName) {
      instances <- NFgens[[generatorName]]$instances
      compiled_instances <- NFgens[[generatorName]]$compiled_instances
      if (!length(instances)) {
        return(invisible(NULL))
      }
      message("Determining setup output names during populate step may be incomplete.")
      # Two categories of setup outputs:
      setupOutputNames <- NFgens[[generatorName]]$nfProc$updatedSetupOutputNames # nf_getSetupOutputNames(NFgens[[generatorName]]$nfGenerator)
      newSetupOutputNames <- NFgens[[generatorName]]$nfProc$updatedNewSetupOutputNames
      
      # Use setupSymTab to determine special types like models
      setupSymTab <- NFgens[[generatorName]]$nfProc$setupSymTab
      
      setupOutputSymbolClasses <- setupOutputNames |>
        lapply(\(x) class(setupSymTab$getSymbol(x))[1]) |>
        unlist()
      isModel <- setupOutputSymbolClasses %in% c("symbolModel")
      setupOutputNames_basic <- setupOutputNames[!isModel]
      setupOutputNames_models <- setupOutputNames[isModel]

      newSetupOutputSymbolClasses <- newSetupOutputNames |>
        lapply(\(x) class(setupSymTab$getSymbol(x))[1]) |>
        unlist()
      isModel <- newSetupOutputSymbolClasses %in% c("symbolModel")
      newSetupOutputNames_basic <- newSetupOutputNames[!isModel]
      newSetupOutputNames_models <- newSetupOutputNames[isModel]

      for (i in seq_along(instances)) {
        inst <- instances[[i]]
        inst_newSetupEnv <- NFgens[[generatorName]]$nfProc$instances_newSetupEnvs[[i]]
        setupOutputList <- setupOutputNames_basic |>
          lapply(\(x) inst[[x]]) |>
          setNames(setupOutputNames_basic)
        if(length(setupOutputNames_models)) 
          setupOutputList <- c(
            setupOutputList,
            setupOutputNames_models |> lapply(\(x) model_get_compiled_internal(inst[[x]])) |> setNames(setupOutputNames_models)
          )
        if(length(newSetupOutputNames_basic))
          setupOutputList <- c(
            setupOutputList,
            newSetupOutputNames_basic |> lapply(\(x) inst_newSetupEnv[[x]]) |> setNames(newSetupOutputNames_basic)
          )
        if(length(newSetupOutputNames_models))
          setupOutputList <- c(
            setupOutputList,
            newSetupOutputNames_models |> lapply(\(x) model_get_compiled_internal(inst_newSetupEnv[[x]])) |> setNames(newSetupOutputNames_models)
          )
        nCompiler::value(compiled_instances[[i]]) <- setupOutputList
        compiled_instances[[i]]$cpp_init_()
      }
      NFgens[[generatorName]]$compiled_instances <<- compiled_instances
    },
    process = function() {
      for (i in seq_along(NFgens)) {
        nfProc <- nimbleFunction_setup_proc(generatorName = names(NFgens)[i])
        nfProc$process()
      }
    },
    get_nComp_units = function() {
      nClass_units <- NFgens |>
        lapply(\(x) x$nfProc$nClassGen) |>
        setNames(names(NFgens))
      model_units <- modelGens |>
        lapply(\(x) x$NCgenerator) |>
        setNames(names(modelGens))
      c(RCfuns, nClass_units, model_units)
    },
    instantiate = function(nCompile_results) {
      # instantiate nClass objects
      # Here every instance of every category needs to be created.
      # Then it needs to be assigned values from a list that replaces with compiled counterparts.
      # The types can be looked up from the nfProc.
      # The whole thing could get a little slow.
      # After all assignments, then we call all cpp_init_ methods for every nimbleFunction object
      for (i in seq_along(NFgens)) {
        generatorName <- names(NFgens)[i]
        if (!generatorName %in% names(nCompile_results)) {
          stop(paste0("No compiled result found for nimbleFunction generator ", generatorName), call. = FALSE)
        }
        nimbleFunction_instantiate(generatorName, nCompile_results[[generatorName]])
      }
      for (i in seq_along(modelGens)) {
        NCgen_label <- names(modelGens)[i]
        if (!NCgen_label %in% names(nCompile_results)) {
          stop(paste0("No compiled result found for model generator ", NCgen_label), call. = FALSE)
        }
        model_instantiate(NCgen_label, nCompile_results[[NCgen_label]])
      }
      ## Populate
      for (i in seq_along(modelGens)) {
        NCgen_label <- names(modelGens)[i]
        model_populate(NCgen_label)
      }
      for (i in seq_along(NFgens)) {
        generatorName <- names(NFgens)[i]
        nimbleFunction_populate(generatorName)
      }
      invisible(NULL)
    }
  )
)
