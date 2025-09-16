
#' Print method for bakerrr S7 job objects
#'
#' Displays a concise summary of the \code{bakerrr} job object, including current status,
#' function name, number of argument sets, daemon count, cleanup setting, process status, and result summary.
#' Outputs a status icon and key runtime information for quick inspection.
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
S7::method(print, bakerrr) <- function(x, ...) {
  status <- if (!is.null(x@bg_job_status)) {
    if (x@bg_job_status$is_alive()) "running"
    else "completed"
  } else "created"

  print_constants <- get_print_constants()

  status_icon <- switch(
    status,
    "created" = print_constants$emojis$created,
    "running" = print_constants$emojis$running,
    "completed" = print_constants$emojis$completed,
    "failed" = print_constants$emojis$failed,
    print_constants$emojis$default
  )

  cat(sprintf("\n%s bakerrr\n", status_icon))
  cat(sprintf("%s Status: %s\n", print_constants$non_ascii_chars$horizontal_t, toupper(status)))
  cat(sprintf("%s Function: %s\n", print_constants$non_ascii_chars$horizontal_t, if (!is.null(x@fun)) deparse1(x@fun)[1] else "<none>"))

  args_len <- length(x@args_list)
  cat(sprintf("%s Args: %d sets\n", print_constants$non_ascii_chars$horizontal_t, args_len))
  cat(sprintf("%s Daemons: %d\n", print_constants$non_ascii_chars$horizontal_t, x@n_daemons))
  cat(sprintf("%s Cleanup: %s\n", print_constants$non_ascii_chars$horizontal_t, ifelse(x@cleanup, "enabled", "disabled")))
  if (!is.null(x@bg_job_status)) {
    cat(sprintf("%s Process alive: %s\n", print_constants$non_ascii_chars$horizontal_t, x@bg_job_status$is_alive()))
  }

  # Results summary
  result <- tryCatch(x@results, error = function(e) NULL)
  if (!is.null(result)) {
    cat(
      glue::glue(
        "{print_constants$non_ascii_chars$horizontal_t} Result:\n"
      )
    )
    cat(sprintf(
      glue::glue(
        .trim = FALSE,
        "\n     {print_constants$non_ascii_chars$horizontal_l} %s"
      ),
      if (is.list(result)) sprintf("List with %d elements", length(result))
      else if (is.character(result)) substr(result, 1, 50)
      else paste("<", class(result)[1], ">", sep = "")
    ))
  }
  cat("\n")
  invisible(x)
}
