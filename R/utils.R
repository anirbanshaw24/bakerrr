
bakerrr_path <- function(..., allow_error = FALSE) {
  fs::path(
    system.file(
      ..., package = "bakerrr", mustWork = allow_error
    )
  )
}

load_config <- function() {
  config_file <- bakerrr_path(
    "constants", "constants.yml", allow_error = TRUE
  )

  if (file.exists(config_file)) {
    config::get(file = config_file)
  } else {
    warning("Config file not found in package.")
    NULL
  }
}

pkg_env <- new.env(parent = emptyenv())

get_print_constants <- function() {
  pkg_env$pkg_constants$print
}
