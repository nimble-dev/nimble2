# nimbleSizeCallNames is needed for nf_checkDSLcode for backward compatibility.
# Checking case-by-case support is a separate question.
# File nimbleSizeCallNames.R is created as follows:
# > paste("nimbleSizeCallNames <-", deparse(nimble:::sizeCalls |> names()) |> paste0(collapse = "\n")) |> writeLines()


# This list is for backward compatibility for nf_checkDSLcode.
# Case-by-base implementation is a separate question.
otherDSLcalls <- c(
  "{",
  "[[",
  "$",
  "resize",
  "declare",
  "returnType",
  "seq_along",
  "double",
  "character",
  "rankSample",
  "new",
  "nimEigen",
  "nimSvd",
  "nimOptim",
  "nimIntegrate",
  "nimOptimDefaultControl",
  "nimDerivs",
  "any_na",
  "any_nan",
  "void"
)

# This list is used for keyword replacement and code checking.
# Implementation compatibility may need case by case attention.
nimKeyWords <- list(
  copy = "nimCopy",
  print = "nimPrint",
  cat = "nimCat",
  step = "nimStep",
  equals = "nimEquals",
  dim = "nimDim",
  stop = "nimStop",
  numeric = "nimNumeric",
  logical = "nimLogical",
  integer = "nimInteger",
  matrix = "nimMatrix",
  array = "nimArray",
  round = "nimRound",
  c = "nimC",
  rep = "nimRep",
  seq = "nimSeq",
  eigen = "nimEigen",
  svd = "nimSvd",
  optim = "nimOptim",
  integrate = "nimIntegrate",
  optimDefaultControl = "nimOptimDefaultControl",
  min.bound = "carMinBound",
  max.bound = "carMaxBound",
  derivs = "nimDerivs"
)

# We retain this list so that nf_checkDSLcode can work
# with backward compatibility. Implementation is another
# question, to be addressed call by call.
specificCallReplacements <- list(
  #    '^' = 'pow', # Has its own handler below
  "%%" = "nimMod",
  length = "size",
  is.nan = "nimIsNaN",
  any_nan = "nimAnyNaN",
  is.na = "nimIsNA",
  any_na = "nimAnyNA",
  lgamma = "lgammafn",
  logfact = "lfactorial",
  loggam = "lgammafn",
  besselK = "bessel_k",
  gamma = "gammafn",
  expit = "ilogit",
  phi = "iprobit",
  ceiling = "ceil",
  trunc = "ftrunc",
  nimDim = "dim",
  checkInterrupt = "R_CheckUserInterrupt"
)

nf_changeNimKeywords <- function(code) {
  if (length(code) > 0) {
    for (i in seq_along(code)) {
      if (is.call(code)) {
        if (!is.null(code[[i]])) {
          code[[i]] <- nf_changeNimKeywordsOne(code[[i]])
        }
      }
    }
  }
  return(code)
}

nf_changeNimKeywordsOne <- function(code, first = FALSE) {
  if (length(code) == 1) {
    if (as.character(code) %in% names(nimKeyWords)) {
      if (is.call(code)) {
        code[[1]] <- as.name(nimKeyWords[[as.character(code)]])
      } else {
        if (!is.character(code) & first) {
          code <- as.name(nimKeyWords[[as.character(code)]])
        }
      }
    }
  } else if (length(code) > 1) {
    for (i in seq_along(code)) {
      if (!is.null(code[[i]])) {
        code[[i]] <- nf_changeNimKeywordsOne(code[[i]], first = i == 1)
      }
    }
  }
  return(code)
}
