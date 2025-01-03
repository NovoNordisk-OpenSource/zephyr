#' Create package option
#'
#' @description
#' Use inside your package to setup a `zephyr_option` that you can use in your
#' functions with `get_option()`. The specification is stored inside the
#' environment of your package.
#'
#' For more information and how to get started see `use_zephyr()`.
#'
#' @param name `[character(1)]` Name of the option
#' @param default `[any]` Default value of the option
#' @param description `[character(1)]` Description of the option
#' @returns Invisible `zephyr_option` object
#' @examplesIf FALSE
#' # Must be run inside a package
#' create_option(
#'   name = "answer",
#'   default = 42,
#'   description = "This is supposed to be the question"
#' )
#' @export
create_option <- function(name, default, description) {
  envir <- parent.frame()

  spec <- structure(
    list(
      default = default,
      name = name,
      description = description,
      environment = envname(envir)
    ),
    class = c("zephyr_option")
  )

  if (!exists(".zephyr_options", envir = envir, inherits = FALSE)) {
    envir[[".zephyr_options"]] <- structure(list(), class = "zephyr_options")
  }

  envir[[".zephyr_options"]][[name]] <- spec

  return(invisible(spec))
}

#' @export
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

#' @export
print.zephyr_option <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
print.zephyr_options <- function(x, ...) {
  lapply(X = x, FUN = print)
  invisible(x)
}
