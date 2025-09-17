
library(testthat)
library(bakerrr)

describe("Jobs are run in background and in parallel", {

  it("run_jobs executes and returns correct results", {
    add <- function(x, y) x + y
    obj <- bakerrr(
      add,
      list(
        list(x = 2, y = 3),
        list(x = 0, y = -1)
      ), n_daemons = 2
    )
    obj <- run_jobs(obj, wait_for_results = TRUE)
    res <- obj@results

    expect_type(res, "list")
    expect_equal(res[[1]], 5)
    expect_equal(res[[2]], -1)
  })

  it("run_jobs handles errors and returns error string", {
    err_fun <- function(x) stop("failtest")
    obj <- bakerrr(err_fun, list(list(x = 1)), n_daemons = 1)
    obj <- run_jobs(obj, wait_for_results = TRUE)

    expect_type(obj@results, "list")
    expect_match(obj@results[[1]], "Error in purrr::in_parallel")
  })
})
