
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
    "created" = print_constants$emojis$created,
    "running" = print_constants$emojis$running,
    "completed" = print_constants$emojis$completed,
    "failed" = print_constants$emojis$failed,
    print_constants$emojis$default
  )

  job_name <- if (!is.null(x@fun))
    deparse(substitute(x@fun))[1]
  else "BackgroundParallelJob"

  cat(
    sprintf(
      "%s %s [%s] - %d daemons, %d jobs\n",
      status_icon, job_name, status, x@n_daemons, length(x@jobs)
    )
  )

  invisible(x)
}
