#' Get value of package option
#'
#' @description
#' Retrieves the value of an `zephyr_option`.
#' The value is looked up in the following order:
#'
#' 1. User defined option: "\{pkgname\}.\{name\}"
#' 1. System variable: "R_\{pkgname\}_\{name\}"
#' 1. Default value defined with `create_option()`
#'
#' And returns the first set value.
#'
#' @details
#' Environment variables are always defined as character strings.
#' In order to return consistent values the following conversions are applied:
#'
#' 1. If they contain a ";" they are split into a vector using ";" as the
#' delimiter.
#' 1. If the class of the default value is not character, the value is converted
#' to the same class using the naive `as.{class}` function. E.g. conversions to
#' numeric are done with `as.numeric()`.
#'
#' These conversions are simple in nature and will not cover advanced cases, but
#' we should try to keep our options this simple.
#'
#' @param name `[character(1)]` Name of the option
#' @param .envir Environment in which the option is defined.
#' Default is suitable for use inside your package.
#' @returns Value of the option
#' @examples
#' # Retrieve the verbosity level option set by default in zephyr:
#' get_option(name = "verbosity_level", .envir = "zephyr")
#' @export
get_option <- function(name, .envir = sys.function(which = -1)) {
  if (!is.character(name) || length(name) > 1) {
    cli::cli_abort("{.var name} must be of class {.cls character} and length {.val 1}")
  }

  env <- envname(.envir)

  default <- NULL
  if (env %in% loadedNamespaces()) {
    default <- getNamespace(env)[[".zephyr_options"]][[name]][["default"]]
  }

  coalesce_dots(
    paste(env, name, sep = ".") |>
      tolower() |>
      getOption(),
    paste("R", env, name, sep = "_") |>
      toupper() |>
      sys_getenv() |>
      fix_env_class(to_class = class(default)),
    default
  )
}

#' @noRd
envname <- function(.envir) {
  if (is.function(.envir)) {
    .envir <- environment(.envir)
  }
  if (is.environment(.envir)) {
    .envir <- environmentName(.envir)
  }
  if (!is.character(.envir) || .envir == "") {
    cli::cli_abort("{.var .envir} must be of class {.cls function}, {.cls environment}, or {.cls character}")
  }
  return(.envir)
}

#' @noRd
sys_getenv <- function(x) {
  x <- Sys.getenv(x)
  if (x == "") {
    return(NULL)
  }
  strsplit(x = x, split = ";") |>
    unlist()
}

#' @noRd
fix_env_class <- function(x, to_class) {
  if (is.null(x) || class(x) == to_class) {
    return(x)
  }
  if (is.character(x) && to_class == "logical") {
    x <- toupper(x)
    x[x %in% c("Y", "YES", "T")] <- "TRUE"
    x[x %in% c("N", "NO", "F")] <- "FALSE"
  }
  do.call(what = paste0("as.", to_class), args = list(x))
}

#' @noRd
coalesce_dots <- function(...) {
  dots <- rlang::list2(...)
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    if (!is.null(dot) && !all(is.na(dot))) {
      return(dot)
    }
  }
  return(NULL)
}
