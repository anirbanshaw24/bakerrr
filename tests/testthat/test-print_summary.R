
library(testthat)
library(bakerrr)

describe("bakerrr is printed correctly", {

  it("print method completes and prints summary", {
    fun <- function(x) x
    obj <- bakerrr(fun, list(list(x = 1)), n_daemons = 1)
    expect_invisible(print(obj))
  })

  it("summary method executes and prints summary", {
    fun <- function(x) x
    obj <- bakerrr(fun, list(list(x = 1)), n_daemons = 1)
    expect_invisible(summary(obj))
  })
})
