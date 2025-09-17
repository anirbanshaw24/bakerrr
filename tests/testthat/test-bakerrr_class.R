
library(testthat)
library(bakerrr)

describe("bakerrr class is behaving as expected", {

  it("bakerrr constructor validates arguments", {
    expect_error(
      bakerrr(fun = "notafun", args_list = list()),
      "*@fun must be <function>, not <character>*"
    )

    expect_error(
      bakerrr(fun = sum, args_list = "notalist"),
      "*@args_list must be <list>, not <character>*"
    )
  })

  it("bakerrr object contains expected properties", {
    fun <- sum
    args_l <- list(list(1:3), list(4:5))
    obj <- bakerrr(fun, args_l)

    expect_s7_class(obj, bakerrr::bakerrr)
    expect_true(is.function(obj@fun))
    expect_type(obj@args_list, "list")
    expect_type(obj@bg_args, "list")
    expect_type(obj@n_daemons, "integer")
  })

  it("job specification creation is correct", {
    fun <- sum
    args_l <- list(list(1:3), list(4:5))
    obj <- bakerrr(fun, args_l)
    jobs <- obj@jobs
    expect_length(jobs, 2)
    expect_equal(jobs[[1]]$fun, sum)
    expect_equal(jobs[[2]]$args, list(4:5))
  })
})
