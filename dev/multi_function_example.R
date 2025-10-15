
devtools::document()
pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE
)

fun_list <- list(
  function(x, y) { Sys.sleep(2); x + y },
  function(x, y) { Sys.sleep(2); x * y },
  function(x, y) { Sys.sleep(2); x - y },
  function(x, y) { Sys.sleep(2); x / y },
  function(x, y) { Sys.sleep(2); x^y },
  function(x, y) { Sys.sleep(2); x %% y },
  function(x, y) { Sys.sleep(2); paste0(x, "-", y) },
  function(x, y) { Sys.sleep(2); mean(c(x, y)) },
  function(x, y) { Sys.sleep(2); max(x, y) },
  function(x, y) { Sys.sleep(2); min(x, y) }
)

args_list <- list(
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = "p", y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 5)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 3)),
  list(x = sample(LETTERS, 1), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10))
)

new_baker <- bakerrr(
  fun = fun_list,
  args_list = args_list,
  n_daemons = 4
  # bg_args = list(
  #   stdout = "out.log",
  #   stderr = "error.log"
  # )
) |>
  run_jobs(wait_for_results = TRUE)

new_baker@results

new_baker |>
  print() |>
  summary() |>
  status()
