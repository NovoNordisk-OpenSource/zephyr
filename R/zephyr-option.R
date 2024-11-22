#' Package-Specific Option Management
#'
#' Set a package-specific option
#'
#' @param package Character. Package name
#' @param option Character. Option name
#' @param value Any. Option value
#' @export
opt_set_pkg <- function(package, option, value) {
  options_key <- paste0(package, ".options")
  pkg_options <- getOption(options_key, default = list())
  pkg_options[[option]] <- value
  options(setNames(list(pkg_options), options_key))
}

#' Get the source of a package-specific option
#'
#' @param package Character. Package name
#' @param option Character. Option name
#' @return Character. Source of the option
#' @export
opt_source_pkg <- function(package, option) {
  options_key <- paste0(package, ".options")
  pkg_options <- getOption(options_key)

  if (is.null(pkg_options) || is.null(pkg_options[[option]])) {
    return(NULL)
  }

  return(options_key)
}

#' Retrieve a package-specific option
#'
#' @param package Character. Package name
#' @param option Character. Option name
#' @return The value of the specified option
#' @export
option_spec_pkg <- function(package, option) {
  options_key <- paste0(package, ".options")
  pkg_options <- getOption(options_key)

  if (is.null(pkg_options) || is.null(pkg_options[[option]])) {
    return(NULL)
  }

  return(pkg_options[[option]])
}

#' Define package option (Generic function)
#'
#' @param package Character. Package name
#' @param option Character. Option name
#' @param ... Additional arguments
#' @export
define_option_pkg <- function(package, option, ...) {
  UseMethod("define_option_pkg")
}

#' Define Zephyr package option
#'
#' @param package Character. Package name
#' @param option Character. Option name
#' @param default Any. Default value
#' @param desc Character. Option description
#' @param envir Environment. Storage environment
#' @export
define_option_pkg.zephyr <- function(package, option, default, desc = NULL,
  envir = getNamespace(package)) {
  options_key <- paste0(package, ".options")
  pkg_options <- getOption(options_key, default = list())
  pkg_options[[option]] <- default
  options(setNames(list(pkg_options), options_key))
}

#' Default method for define_option_pkg
#'
#' @export
define_option_pkg.default <- function(package, option, ...) {
  opt_set_pkg(package, option, ...)
}
