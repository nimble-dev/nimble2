#' @export
#' @importFrom nimbleModel values
values <- function(model, nodes) {
  nimbleModel::values(model, nodes)
}

#' @export
#' @importFrom nimbleModel `values<-`
`values<-` <- function(model, nodes, value) {
  nimbleModel::`values<-`(model, nodes, value)
}
