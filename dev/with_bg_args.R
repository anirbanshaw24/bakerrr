
devtools::document()
pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE
)

# Example: Data processing pipeline
process_data <- function(dataset_id, filter_col, threshold) {
  # Simulate data processing with realistic timing
  Sys.sleep(runif(1, 0.5, 2))

  # Simulate processing logic
  result_size <- sample(100:1000, 1)

  to_return <- list(
    dataset_id = dataset_id,
    records_processed = result_size,
    filter_applied = paste(filter_col, ">=", threshold),
    timestamp = Sys.time()
  )
  # Print to log
  print(to_return)
  to_return
}

# Generate realistic argument sets
args_list <- purrr::map(1:12, ~ list(
  dataset_id = paste0("DS_", .x),
  filter_col = sample(c("score", "value", "rating"), 1),
  threshold = sample(50:95, 1)
))

# Add intentional error case for demonstration
args_list[[5]] <- list(
  dataset_id = NULL,  # Will cause error
  filter_col = "score",
  threshold = 75
)


# Create and run jobs with optimal daemon count
stirr_job <- bakerrr::bakerrr(
  fun = process_data,
  args_list = args_list,
  n_daemons = min(4, length(args_list)),  # Adaptive daemon count
  bg_args = list(
    stdout = "bakerrr_out.log",
    stderr = "bakerrr_err.log"
  )
)

# Execute with real-time monitoring
stirr_job <- stirr_job |>
  bakerrr::run_jobs(wait_for_results = TRUE)

stirr_job@results
