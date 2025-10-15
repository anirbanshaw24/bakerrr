
#' Summary method for bakerrr S7 job objects
#'
#' Provides a one-line summary of a \code{bakerrr} job object,
#' indicating the status,
#' function name, number of worker daemons, and total jobs.
#' Prints a status icon and brief job info.
#'
#' @param x A \code{bakerrr} S7 job object.
#' @param ... Additional arguments (currently ignored).
#'
#' @importFrom S7 new_generic method
#'
#' @export
#'
#' @return The input \code{x}, invisibly, after printing the summary.
summary <- S7::new_generic("summary", "x")
S7::method(summary, bakerrr) <- function(x, ...) {
  print_constants <- get_print_constants()

  status <- if (!is.null(x@bg_job_status)) {
    if (x@bg_job_status$is_alive()) "running"
    else "completed"
  } else {
    "created"
  }

  status_icon <- switch(
    status,
    "created"   = print_constants$emojis$created,
    "running"   = print_constants$emojis$running,
    "completed" = print_constants$emojis$completed,
    "failed"    = print_constants$emojis$failed,
    print_constants$emojis$default
  )

  funs <- if (is.function(x@fun)) rep(list(x@fun), length(x@args_list)) else x@fun
  num_funs <- length(funs)
  num_jobs <- length(x@args_list)

  cat(sprintf(
    "%s bakerrr | Status: %s | Functions: %d | Daemons: %d | Jobs: %d\n",
    status_icon, toupper(status), num_funs, x@n_daemons, num_jobs
  ))

  invisible(x)
}
