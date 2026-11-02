# Function forms of model operations, as in nimble: e.g. calculate(model, nodes)
# for model$calculate(nodes). These are used when a nimbleFunction runs uncompiled.
# In compiled code, keyword processing handles them.
# nimble's internal arguments nodeFxnVector and nodeFunctionIndex are not supported.

#' Operations on model nodes
#'
#' Calculate, calculate the difference, get the log probabilities of, or simulate
#' nodes of a model. Each is equivalent to the model method of the same name,
#' e.g. \code{calculate(model, nodes)} is \code{model$calculate(nodes)}.
#'
#' @param model a nimble model, compiled or uncompiled.
#' @param nodes character vector of node names, or other input accepted by the model
#'   methods. If missing, all nodes in the model are used.
#' @param includeData for simulate, whether data nodes are simulated.
#'
#' @return calculate and getLogProb return the sum of the log probabilities (densities)
#'   of the nodes, with a contribution of 0 from any deterministic nodes.
#'   calculateDiff returns the sum of the differences between the new and old log
#'   probabilities (densities) of the nodes. simulate returns NULL invisibly.
#'
#' @rdname nodeFunctions
#' @export
calculate <- function(model, nodes) {
  if (missing(nodes)) model$calculate() else model$calculate(nodes)
}

#' @rdname nodeFunctions
#' @export
calculateDiff <- function(model, nodes) {
  if (missing(nodes)) model$calculateDiff() else model$calculateDiff(nodes)
}

#' @rdname nodeFunctions
#' @export
getLogProb <- function(model, nodes) {
  if (missing(nodes)) model$getLogProb() else model$getLogProb(nodes)
}

#' @rdname nodeFunctions
#' @export
simulate <- function(model, nodes, includeData = FALSE) {
  if (missing(nodes)) model$simulate(includeData = includeData) else model$simulate(nodes, includeData = includeData)
}

#' Get a parameter or bound of a stochastic model node
#'
#' \code{getParam(model, node, param)} is equivalent to \code{model$getParam(node, param)},
#' and \code{getBound(model, node, bound)} to \code{model$getBound(node, bound)}.
#'
#' @param model a nimble model, compiled or uncompiled.
#' @param node the name of a stochastic node. If it names more than one node, the first is used.
#' @param param name of a parameter of the node's distribution.
#' @param bound \code{"lower"} or \code{"upper"}.
#'
#' @return The value of the parameter or bound.
#'
#' @rdname getParam
#' @export
getParam <- function(model, node, param) {
  model$getParam(node, param)
}

#' @rdname getParam
#' @export
getBound <- function(model, node, bound) {
  model$getBound(node, bound)
}
