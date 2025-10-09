
#' Print method for bakerrr S7 job objects
#'
#' Displays a concise summary of the \code{bakerrr} job object,
#' including current status,
#' function name, number of argument sets, daemon count,
#' cleanup setting, process status, and result summary.
#' Outputs a status icon and key runtime information for
#' quick inspection.
#'
#' @name print
#'
#' @param x A \code{bakerrr} S7 job object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return The input \code{x}, invisibly, after printing the summary.
#'
#' @importFrom S7 method
#'
#' @export
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

  # Function summary: list or single
  fun_summary <- if (is.list(x@fun) && all(purrr::map_lgl(x@fun, is.function))) {
    sprintf("<list of %d functions>", length(x@fun))
  } else if (is.function(x@fun)) {
    # Use first line of body, trim if long
    fun_str <- paste(deparse(x@fun), collapse = " ")
    if (nchar(fun_str) > 40) fun_str <- paste0(substr(fun_str, 1, 37), "...")
    fun_str
  } else {
    "<none>"
  }

  cat(
    sprintf(
      "%s %s [%s] - %d daemons, %d jobs\n",
      status_icon, fun_summary, status, x@n_daemons, length(x@args_list)
    )
  )

  invisible(x)
}
