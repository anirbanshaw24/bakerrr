#' Status method for bakerrr S7 job objects
#'
#' Returns job status as "waiting", "running", or "done".
#'
#' @param x A \code{bakerrr} S7 job object.
#' @param ... Further arguments (ignored).
#'
#' @return One of "waiting", "running", "done".
#'
#' @export
status <- S7::new_generic("status", "x")
S7::method(status, bakerrr) <- function(x, ...) {
  if (is.null(x@bg_job_status)) {
    "waiting"
  } else if (x@bg_job_status$is_alive()) {
    "running"
  } else {
    "done"
  }
}
