
pkgload::load_all(
  export_all = FALSE,
  attach_testthat = FALSE
)

long_stat_calc <- function(x, n_boot, sleep_time) {
  # x: numeric vector
  # n_boot: number of bootstraps
  # sleep_time: pause after each bootstrap (sec)

  if (!is.numeric(x)) stop("Input x must be numeric.")
  if (length(x) < 2) stop("Input x must have at least 2 values.")

  start_time <- Sys.time()
  boot_means <- numeric(n_boot)

  for (i in seq_len(n_boot)) {
    boot_means[i] <- mean(sample(x, replace = TRUE))
    if (sleep_time > 0) Sys.sleep(sleep_time) # Simulate long computation
  }

  end_time <- Sys.time()

  result <- list(
    boot_mean = mean(boot_means),
    boot_sd   = sd(boot_means),
    elapsed   = difftime(end_time, start_time, units = "secs")
  )
  class(result) <- "long_stat_calc"
  return(result)
}

# print method for easy reporting
print.long_stat_calc <- function(x, ...) {
  cat("Bootstrap Mean:", x$boot_mean, "\n")
  cat("Bootstrap SD:  ", x$boot_sd, "\n")
  cat("Elapsed Time:  ", x$elapsed, "seconds\n")
}

args_list <- list(
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002),
  list(rnorm(100), n_boot = 3000, sleep_time = 0.002)
)



mirai::daemons(0)
set.seed(10)
system.time({
  mirai::daemons(6)
  res <- mirai::mirai_map(
    .x = list(
      rnorm(100), rnorm(100), rnorm(100), rnorm(100),
      rnorm(100), rnorm(100), rnorm(100), rnorm(100),
      rnorm(100), rnorm(100)
    ),
    .f = long_stat_calc,
    .args = list(
      n_boot = 3000, sleep_time = 0.002
    )
  )
  res[.progress]
  mirai_res <<- res[.flat]
})


system.time({
  new_baker <- bakerrr::bakerrr(
    long_stat_calc,
    args_list = args_list,
    n_daemons = 6
    # bg_args = list(
    #   stdout = "out.log",
    #   stderr = "error.log"
    # )
  ) |>
    bakerrr::run_jobs(wait_for_results = TRUE)
  bakerrr_res <<- new_baker@results
})
