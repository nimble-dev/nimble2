# There are more tests of DSL checking in nimble_tests / test-checkDSL.R
# Here are some quick tests of a new internal function used within DSL checking.

test_that("basic DSL checking tools work", {
  # see also some nimble tests
  findMethodsInRcode <- nimble2:::findMethodsInRcode
  
  # The key code recursion tool used to find what
  # look like method names to avoid flagging them as
  # potential errors is findMethodsInRcode.
  # This replaced nimble's fineMethodsInExprClass
  expect_equal(
    findMethodsInRcode(quote(a$f(x))),
    "f")
  expect_equal(
    findMethodsInRcode(quote(a(b$f(), c$d(e$z(h)), p$q, xya, 2))),
    c("f", "d", "z")) # not q
  expect_equal(
    findMethodsInRcode(
      quote(for(i in 1:5) {res[i] <- foo(b$f(x[2]))})),
    "f")
  expect_equal(
    findMethodsInRcode(
      quote(
        if(x[i] == 6) {
          z <- foo(b); z <- b$h(z)
        } else z <- p$g$j$q(x))),
    c("h", "q")) # not g or j
  expect_equal(
    findMethodsInRcode(quote(x[i]$y())),
    "y")
  expect_equal(
    findMethodsInRcode(quote(x[[i]]$y())),
    "y")
  expect_equal(
    findMethodsInRcode(quote(a$b(x)$foo())),
    c("foo", "b"))
})
