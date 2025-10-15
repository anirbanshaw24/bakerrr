
bakerrr <- S7::new_class(
  "bakerrr",
  package = "bakerrr",
  properties = list(
    fun = S7::class_any,       # Accepts function OR list of functions
    args_list = S7::class_list,
    bg_args = S7::class_list,
    jobs = S7::new_property(
      class = S7::class_list,
      getter = function(self) {
        funs <- if (is.function(self@fun)) {
          rep(list(self@fun), length(self@args_list))
        } else if (is.list(self@fun) && all(purrr::map_lgl(self@fun, is.function))) {
          self@fun
        } else {
          stop("`fun` must be a function or list of functions")
        }
        purrr::map2(
          funs, self@args_list,
          ~ list(fun = .x, args = .y)
        )
      }
    ),
    bg_job_status = S7::class_any,
    results = S7::new_property(
      class = S7::class_list,
      getter = function(self) {
        if (is.null(self@bg_job_status)) {
          glue::glue(" - bakerrr:: Job not started. Start job by calling run_jobs function.")
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
    # Standardize fun to a list if single function
    funs <- if (is.function(fun)) {
      rep(list(fun), length(args_list))
    } else if (is.list(fun) && all(purrr::map_lgl(fun, is.function))) {
      fun
    } else {
      stop("`fun` must be a function or list of functions")
    }
    # Validation: function/args_list length match
    if (length(funs) != length(args_list)) {
      stop(
        glue::glue(
          "`fun` (length={length(funs)}) and `args_list` (length={length(args_list)}) must have the same length"
        )
      )
    }
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
    funs <- if (is.function(self@fun)) {
      rep(list(self@fun), length(self@args_list))
    } else if (is.list(self@fun) && all(purrr::map_lgl(self@fun, is.function))) {
      self@fun
    } else {
      return("@fun must be a function or list of functions.")
    }
    if (!is.list(self@args_list)) {
      return("@args_list must be a list.")
    }
    if (length(funs) != length(self@args_list)) {
      return("Length of @fun and @args_list must match.")
    }
    # Check each fun-args combo for required formal arguments
    arg_validation <- purrr::map2_chr(funs, self@args_list, function(f, args, i) {
      fn_formals <- names(formals(f))
      missing_args <- setdiff(fn_formals, names(args))
      if (length(missing_args) > 0) {
        glue::glue(
          "Function at index {i} ({deparse(substitute(f))}): missing required arguments: {paste(missing_args, collapse=', ')}"
        )
      } else {
        NA_character_
      }
    }, .id = "i")
    arg_validation <- arg_validation[!is.na(arg_validation)]
    if (length(arg_validation) > 0) {
      return(paste(arg_validation, collapse = "\n"))
    }
    if (!is.numeric(self@n_daemons)) {
      return("@n_daemons MUST be Numeric")
    }
  }
)
