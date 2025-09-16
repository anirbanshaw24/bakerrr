
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

  status_icon <- switch(status,
                        "created" = "🔄", "running" = "⏳",
                        "completed" = "✅", "failed" = "❌", "🔍"
  )

  cat(sprintf("\n%s bakerrr\n", status_icon))
  cat(sprintf("├─ Status: %s\n", toupper(status)))
  cat(sprintf("├─ Function: %s\n", if (!is.null(x@fun)) deparse1(x@fun)[1] else "<none>"))

  args_len <- length(x@args_list)
  cat(sprintf("├─ Args: %d sets\n", args_len))
  cat(sprintf("├─ Daemons: %d\n", x@n_daemons))
  cat(sprintf("├─ Cleanup: %s\n", ifelse(x@cleanup, "enabled", "disabled")))
  if (!is.null(x@bg_job_status)) {
    cat(sprintf("├─ Process alive: %s\n", x@bg_job_status$is_alive()))
  }

  # Results summary
  result <- tryCatch(x@results, error = function(e) NULL)
  if (!is.null(result)) {
    cat("├─ Result:\n")
    cat(sprintf("│  └─ %s\n",
                if (is.list(result)) sprintf("List with %d elements", length(result))
                else if (is.character(result)) substr(result, 1, 50)
                else paste("<", class(result)[1], ">", sep = "")
    ))
  }
  cat("\n")
  invisible(x)
}
