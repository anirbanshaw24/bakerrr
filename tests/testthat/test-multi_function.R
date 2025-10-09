
describe("bakerrr parallel job orchestration with list of functions", {

  set.seed(42)
  fun_list <- list(
    function(x, y) { Sys.sleep(0.1); x + y },
    function(x, y) { Sys.sleep(0.1); x * y },
    function(x, y) { Sys.sleep(0.1); x - y },
    function(x, y) { Sys.sleep(0.1); x / y },
    function(x, y) { Sys.sleep(0.1); x^y },
    function(x, y) { Sys.sleep(0.1); x %% y },
    function(x, y) { Sys.sleep(0.1); paste0(x, "-", y) },
    function(x, y) { Sys.sleep(0.1); mean(c(x, y)) },
    function(x, y) { Sys.sleep(0.1); max(x, y) },
    function(x, y) { Sys.sleep(0.1); min(x, y) }
  )

  args_list <- list(
    list(x = 5, y = 3),                  # OK: addition
    list(x = "p", y = 7),                # ERROR: character * numeric
    list(x = 6, y = 8),                  # OK: subtraction
    list(x = 10, y = 2),                 # OK: division
    list(x = 4, y = 2),                  # OK: power
    list(x = 7, y = 2),                  # OK: modulo
    list(x = "Q", y = 5),                # OK: paste
    list(x = 2, y = 8),                  # OK: mean
    list(x = 13, y = 6),                 # OK: max
    list(x = 4, y = 8)                   # OK: min
  )

  it("runs all functions, propagates error, and returns results", {
    new_baker <- bakerrr::bakerrr(
      fun = fun_list,
      args_list = args_list,
      n_daemons = 3
    ) |> run_jobs(wait_for_results = TRUE)

    results <- new_baker@results
    # 1. Result type and length
    expect_type(results, "list")
    expect_length(results, 10)

    # 2. Error propagation: job 2
    expect_true(any(grepl("Error", results[[2]])))
    expect_false(inherits(results[[2]], "try-error"))

    # 3. Check correct answers for numeric jobs
    expect_equal(results[[1]], 8L)
    expect_equal(results[[3]], -2L)
    expect_equal(results[[4]], 5)
    expect_equal(results[[5]], 16)
    expect_equal(results[[6]], 1)
    expect_equal(results[[8]], 5)
    expect_equal(results[[9]], 13)
    expect_equal(results[[10]], 4)

    # 4. String operations result type (job 7: paste)
    expect_type(results[[7]], "character")
    expect_true(grepl("Q-5", results[[7]]))

    # 5. Print summary/status methods do not error
    expect_error(print(new_baker), NA)
    expect_error(summary(new_baker), NA)
    expect_error(status(new_baker), NA)
  })

  it("validates argument/function length mismatch at construction", {
    # 12 functions, 10 args: error expected
    expect_error(
      bakerrr::bakerrr(fun = rep(fun_list, 2), args_list = args_list, n_daemons = 2),
      "must have the same length"
    )
  })

  it("validates proper error if required argument missing", {

    # tryCatch({
    #   bakerrr::bakerrr(fun = fun_list, args_list = rep(list(list(x=1)), 10), n_daemons = 2)
    # }, error = function(err_msg) {
    #   browser()
    # })
    browser()
    expect_error(
      bakerrr::bakerrr(fun = fun_list, args_list = rep(list(list(x=1)), 10), n_daemons = 2),
      "Error: <bakerrr::bakerrr> object is invalid: - Function at index 1 (function (x, y) ): missing required arguments: y"
    )
  })

  it("detects failure if fun is not function/list", {
    expect_error(
      bakerrr::bakerrr(fun = 123, args_list = args_list, n_daemons = 2),
      "must be a function or list of functions"
    )
  })

  it("reports correct status before, during, and after execution", {
    baker <- bakerrr::bakerrr(fun = fun_list, args_list = args_list, n_daemons = 2)
    # Pre-run
    expect_match(suppressMessages(capture.output(status(baker))), "created", all = FALSE)

    baker <- baker |> run_jobs(wait_for_results = FALSE)
    # During (may briefly be running)
    expect_true(status(baker) %in% c("running", "completed"))
  })
})
