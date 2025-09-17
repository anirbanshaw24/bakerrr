
#' S7 bakerrr class for job orchestration and background processing
#'
#' Defines the \code{bakerrr} S7 class for parallel and
#' background job execution.
#' Stores the function to run (\code{fun}),
#' argument lists (\code{args_list}),
#' background job arguments (\code{bg_args}),
#' job objects, results, and runtime properties.
#' Supports retrieval of job status/results and
#' validation of provided properties.
#'
#' @param fun Function to be executed for each job.
#' @param args_list List of argument sets for each job.
#' @param bg_args List of arguments passed to background job handler.
#' @param n_daemons Number of parallel workers (default: ceiling of cores/5).
#' @param cleanup Logical; whether to clean up jobs after
#' execution (default: TRUE).
#'
#' @return An S7 bakerrr class object with job orchestration
#' methods and properties.
#'
#' @import S7
#' @import carrier
#' @importFrom parallel detectCores
#'
#' @examples
#' # Create a bakerrr object to process jobs in parallel
#' bakerrr::bakerrr(fun = sum, args_list = list(list(1:10), list(10:20)))
#'
#' @export
bakerrr <- S7::new_class(
  "bakerrr",
  package = "bakerrr",
  properties = list(
    fun = S7::class_function,
    args_list = S7::class_list,
    bg_args = S7::class_list,
    jobs = S7::new_property(
      class = S7::class_list,
      getter = function(self) {
        purrr::map(
          self@args_list,
          ~ list(fun = self@fun, args = .x)
        )
      }
    ),
    bg_job_status = S7::class_any,
    results = S7::new_property(
      class = S7::class_list,
      getter = function(self) {
        if (is.null(self@bg_job_status)) {
          glue::glue(
            " - bakerrr:: Job not started. ",
            "Start job by calling run_jobs function."
          )
        } else if (
          !is.null(self@bg_job_status) &&
            !self@bg_job_status$is_alive()
        ) {
          self@bg_job_status$get_result()
        } else {
          self@bg_job_status$get_status()
        }
      }
    ),
    n_daemons = S7::class_integer,
    cleanup = S7::class_logical
  ),
  constructor = function(
    fun, args_list, bg_args = list(),
    n_daemons = ceiling(parallel::detectCores() / 5),
    cleanup = TRUE
  ) {
    S7::new_object(
      S7::S7_object(),
      fun = fun,
      args_list = args_list,
      bg_args = bg_args,
      n_daemons = as.integer(n_daemons),
      cleanup = cleanup
    )
  },
  validator = function(self) {
    if (!is.function(self@fun)) {
      "@fun MUST be a Function."
    } else if (!is.list(self@args_list)) {
      "@args_list MUST be a List"
    } else if (!is.numeric(self@n_daemons)) {
      "@n_daemons MUST be Numeric"
    }
  }
)
