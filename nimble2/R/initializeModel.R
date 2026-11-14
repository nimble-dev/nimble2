initializeModel <- nimbleFunction(
  name = 'initializeModel',
  setup = function(model, silent = FALSE) {
    RHSonlyVars <- model$getNodes(includeRHSonly=TRUE, RHSonlyOnly = TRUE)
    RHSonlyVarsNames <- unlist(lapply(RHSonlyVars, \(x) x$toVarChars()))
    numRHS <- length(RHSonlyVars)

    nodes <- model$getNodes(nodesAsChars = TRUE, .sort = TRUE)
    numNodes <- length(nodes)

    stochNodes <- model$getNodes(stochOnly = TRUE, includeData = FALSE, nodesAsChars = TRUE)
    determNodes <- model$getNodes(determOnly = TRUE, nodesAsChars = TRUE)

    typeCode <- rep(2, numNodes)
    typeCode[nodes %in% determNodes] <- 1
    typeCode[nodes %in% stochNodes] <- 3

  },
  run = function() {
    print(numNodes)
    if(numRHS > 0)
      for(i in 1:numRHS)
        check_rhs(i)
    if(numNodes > 0)
      for(i in 1:numNodes) {
        if(typeCode[i] == 1) {
          initialize_determ(i)
        } else {
          if(typeCode[i] == 2) {
            initialize_data(i)
          } else initialize_stoch_nondata(i)
        }
      }
  },
  methods = list(
    check_rhs = function(i = 'integerScalar') {
      if(any_na(values(model, RHSonlyVars[i], mode='raw')))
        nMessage("Right-hand-side only variable `", RHSonlyVarsNames[i], "` has a value of NA or NaN.", level=WARN) 
    },
    initialize_determ = function(i = 'integerScalar') {
      model$calculate(nodes[i])
      if(any_na(values(model, nodes[i], mode='node')))
        nMessage("Deterministic node `", nodes[i], "` has a value of NA or NaN even after running `calculate`.", level=WARN)
    },
    initialize_data = function(i = 'integerScalar') {
      if(any_na(values(model, nodes[i], mode='node')))
        nMessage("Data node `", nodes[i], "` has a value of NA or NaN.", level=WARN)
      lp <- model$calculate(nodes[i])
      if(is_na(lp)) {
        nMessage("Data node `", nodes[i], "` has a logProb of NA or NaN.", level=WARN)
      } else {
        if(lp < -1e12) 
          nMessage("Data node `", nodes[i], "` logProb is ", lp, ".", level=WARN)
      }
    },
    initialize_stoch_nondata = function(i = 'integerScalar') {
      if(any_na(values(model, nodes[i], mode='node'))) {
        # Need to include data so dataNodes not dropped and ordering and length are correct.
        model$simulate(nodes[i], includeData = TRUE)  
        if(any_na(values(model, nodes[i], mode='node')))
          nMessage("Node `", nodes[i], "` has a value of NA or NaN even after running `simulate`.", level=WARN)
      }
      lp <- model$calculate(nodes[i])
      if(is_na(lp)) {
        nMessage("Node `", nodes[i], "` has a logProb of NA or NaN.", level=WARN)
      } else {
        if(lp < -1e12) 
          nMessage("Node `", nodes[i], "` logProb is ", lp, ".", level=WARN)
      }
    }
  )
)

