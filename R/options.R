#' Create package option
#' @description
#' Use inside your package to setup a `zephyr_option` that you can use in your
#' functions with [get_option()]. The specification is stored inside the
#' environment of your package.
#'
#' For more information and how to get started see [use_zephyr()].
#'
#' @param name `[character(1)]` Name of the option
#' @param default `[any]` Default value of the option
#' @param description `[character(1)]` Description of the option
#' @param .envir Environment in which the option is defined.
#' Default is suitable for use inside your package.
#' @returns Invisible `zephyr_option` object
#' @examplesIf FALSE
#' create_option(
#'   name = "answer",
#'   default = 42,
#'   description = "This is supposed to be the question"
#' )
#' @export
create_option <- function(name, default, description = name,
                          .envir = parent.frame()) {
  spec <- structure(
    list(
      default = default,
      name = name,
      description = description,
      environment = envname(.envir)
    ),
    class = c("zephyr_option")
  )

  if (!exists(".zephyr_options", envir = .envir, inherits = FALSE)) {
    .envir[[".zephyr_options"]] <- structure(list(), class = "zephyr_options")
  }

  .envir[[".zephyr_options"]][[name]] <- spec

  return(invisible(spec))
}

#' @noRd
format.zephyr_option <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h3(x$name)
    cli::cli_text(x$description)
    cli::cli_ul(
      c(
        "Default: {.code {deparse1(x$default)}}",
        "Option: {.code {tolower(x$environment)}.{x$name}}",
        "Environment: {.code R_{toupper(x$environment)}_{toupper(x$name)}}"
      )
    )
  })
}

#' @noRd
print.zephyr_option <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @noRd
print.zephyr_options <- function(x, ...) {
  lapply(X = x, FUN = print)
  invisible(x)
}

#' Get value of package option
#' @description
#' Retrieves the value of an `zephyr_option`.
#' The value is looked up in the following order:
#'
#' 1. User defined option: `{pkgname}.{name}`
#' 1. System variable: `R_{PKGNAME}_{NAME}`
#' 1. Default value defined with [create_option()]
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
#' numeric are done with [as.numeric()].
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
    cli::cli_abort(
      "{.var name} must be of class {.cls character} and length {.val 1}"
    )
  }

  env <- envname(.envir)

  default <- NULL
  if (env %in% loadedNamespaces()) {
    default <- getNamespace(env)[[".zephyr_options"]][[name]][["default"]]
  } else if (is.environment(.envir)) {
    default <- .envir[[".zephyr_options"]][[name]][["default"]]
  }

  coalesce_dots(
    paste(env, name, sep = ".") |>
      tolower() |>
      getOption(),
    paste("R", env, name, sep = "_") |>
      toupper() |>
      sys_getenv() |>
      fix_env_class(default = default),
    default
  )
}

#' List package options
#' @description
#' List all `zephyr_options` specified in a package. Either as an `list` or as
#' as `character` vector formatted for use in your package documentation.
#'
#' To document your options use [use_zephyr()] to set everything up, and edit
#' the created template as necessary.
#'
#' @param as `[character(1)]` Format in which to return the options:
#' * `"list"`: Return a nested list, where each top level element is a list with
#' the specification of an option.
#' * `"params"`: Return a character vector with the `"@param"` tag entries for
#' each option similar to how function parameters are documented with roxygen2.
#' * `"markdown"`: Return a character string with markdown formatted entries for
#' each option.
#' @param .envir Environment in which the options are defined.
#' Default is suitable for use inside your package.
#' @returns `list` or `character` depending on `as`
#' @examples
#' # List all options in zephyr
#' x <- list_options(.envir = "zephyr")
#' print(x)
#' str(x)
#'
#' # Create @params tag entries for each option
#' list_options(as = "params", .envir = "zephyr") |>
#'   cat()
#'
#' # List options in markdown format
#' list_options(as = "markdown", .envir = "zephyr") |>
#'   cat()
#' @export
list_options <- function(as = c("list", "params", "markdown"),
                         .envir = sys.function(which = -1)) {
  as <- rlang::arg_match(as)

  env <- envname(.envir)

  options <- list()
  if (env %in% loadedNamespaces()) {
    options <- getNamespace(env)[[".zephyr_options"]]
  } else if (is.environment(.envir)) {
    options <- .envir[[".zephyr_options"]]
  }

  switch(
    EXPR = as,
    "list" = options,
    "params" = options |>
      vapply(
        FUN = glue::glue_data,
        FUN.VALUE = character(1),
        "@param {name} {description}. Default: `{default}`.",
        USE.NAMES = FALSE
      ),
    "markdown" = options |>
      vapply(
        FUN = glue::glue_data,
        FUN.VALUE = character(1),
        "
        ## {name}
        {description}
        * Default: `{default}`
        * Option: `{tolower(environment)}.{name}`
        * Environment: `R_{toupper(environment)}_{toupper(name)}`
        ",
        USE.NAMES = FALSE
      ) |>
      paste(collapse = "\n")
  )
}
