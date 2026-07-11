# Initial rough drafting of modelValues

# mv_nC <- nClass(
#   Cpublic = list(
#     classname = "modelValues",
#     mu = "nList(numericVector())",
#     cov = "nList(numericMatrix())",
#     sizes = 'SEXP',
#     m_ = 'integerScalar',
#     set_sizes = nFunction(
#       function(new_sizes) {
#         sizes <<- new_sizes
#       }
#     ),
#     resize = nFunction(
#       function(m = 'integerScalar') {
#         length(mu) <<- m
#         length(cov) <<- m
#         for(i in )

#         m_ <<- m

#       }
#     ),
#     getsize = nFunction(
#       function() {return(m_); returnType('integerScalar')}
#     )

#   )
# )

# comp <- nCompile(mv_nC, sizes_nC, returnList = TRUE)

# obj <- comp$mv_nC$new()
# obj$sizes <- comp$sizes_nC$new()
# obj$mu <- list(1:3, 2:4)
# obj$mu |> as.list()
# obj$sizes$mu
# obj$set_sizes(list(mu = c(3), cov = c(3,3)))
# obj$sizes$mu
# obj$sizes$cov
# class(obj)
# obj[["mu"]][[1]] <- 5:7
# obj[["mu"]] |> as.list()
# obj["mu"]
