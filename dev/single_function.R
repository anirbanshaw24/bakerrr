
devtools::document()
pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE
)

fun <- function(x, y) {
  Sys.sleep(2)
  x + y
}

args_list <- list(
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = "p", y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10)),
  list(x = ceiling(rnorm(1) * 10), y = ceiling(rnorm(1) * 10))
)

new_baker <- bakerrr(
  fun,
  args_list,
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


