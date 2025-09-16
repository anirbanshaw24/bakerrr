
#' Background parallel processing of jobs using purrr and tryCatch
#'
#' Executes a list of job specifications in parallel,
#' applies the given function with error handling,
#' and collects the results or error messages.
#'
#' @param jobs A list of job specifications, each containing
#' a function (\code{fun}) and arguments (\code{args}).
#'
#' @importFrom glue glue
#' @importFrom purrr imap in_parallel
#'
#' @return A list of results, with error messages in case of failure.
bg_func <- function(jobs) {
  purrr::imap(
    jobs,
    purrr::in_parallel(
      \(x, y) {
        tryCatch({
          do.call(x$fun, x$args)
        },
        error = function(e) {
          glue::glue(
            "Error in purrr::in_parallel: {conditionMessage(e)}"
          )
        })
      }
    )
  )
}

#' Generic for running background jobs in bakerrr
#'
#' Initiates background execution for a bakerrr job object,
#' launching jobs via \code{callr::r_bg}.
#' Stores process status in \code{bg_job_status}.
#'
#' @param x A \code{bakerrr} S7 job object.
#' @param ... Not used. For future expansion.
#'
#' @importFrom callr r_bg
#' @importFrom S7 new_generic method
#'
#' @return The input \code{x}, invisibly, after launching background jobs.
run_bg <- S7::new_generic("run_bg", "x")
S7::method(run_bg, bakerrr) <- function(x) {
  x@bg_job_status <- do.call(
    callr::r_bg, args = c(
      list(
        func = bg_func, args = list(x@jobs)
      ),
      x@bg_args
    )
  )
  invisible(x)
}

#' Run bakerrr jobs and wait for completion
#'
#' Launches the parallel jobs, manages daemon setup (\code{mirai::daemons}),
#' initiates background jobs, provides a console spinner for progress,
#' and optionally waits for results. Cleans up daemons after execution.
#'
#' @param job A \code{bakerrr} S7 job object.
#' @param wait_for_results Logical; whether to block
#' and wait for completion (default: TRUE).
#' @param ... Not used. For future expansion.
#'
#' @return The updated \code{bakerrr} job object, invisibly.
#'
#' @importFrom S7 new_generic method
#' @importFrom mirai daemons
#' @importFrom cli make_spinner
#'
#' @export
run_jobs <- S7::new_generic("run_jobs", c("job", "wait_for_results"))
S7::method(
  run_jobs, list(bakerrr, S7::class_logical)
) <- function(job, wait_for_results) {

  mirai::daemons(job@n_daemons)

  job <- job |> run_bg()

  if (wait_for_results) {
    console_spinner <- cli::make_spinner(
      "clock",
      template = glue::glue(
        " - bakerrr:: Waiting for background ",
        "parallel job to finish... {{spin}}"
      )
    )

    while (TRUE) {
      if (!is.null(job@bg_job_status) && !job@bg_job_status$is_alive()) {
        console_spinner$finish()
        break
      } else {
        console_spinner$spin()
        Sys.sleep(0.25)
      }
    }
  }

  mirai::daemons(0)
  invisible(job)
}
