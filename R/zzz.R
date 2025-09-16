
.onLoad <- function(...) {
  S7::methods_register()
}

.onAttach <- function(libname, pkgname) {
  constants <- load_config()
  if (!is.null(constants)) {
    assign("pkg_constants", constants, envir = pkg_env)
  }
}
