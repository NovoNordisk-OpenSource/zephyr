#' Write messages based on verbosity level
#'
#' @description
#' The `msg()` function is a general utility function for writing messages to the
#' console based on the verbosity level set for your session and package.
#'
#' Verbosity level can be any of the four values below:
#'
#' 1. `quiet`: No messages are displayed.
#' 2. `minimal`: Only essential messages are displayed. As default `msg()` will
#' write messages at this level and higher.
#' 3. `verbose`: More informative messages are displayed. Use `msg_verbose()` to
#' easily write messages at this level and above.
#' 4. `debug`: Detailed messages for debugging are displayed. Use `msg_debug()`
#' to report debug messages.
#'
#' For simple messages in your functions the recommended approach is to use the
#' following wrappers for consistency across packages:
#'
#' - `msg_success()`: To indicate a successful operation. Wrapper around `msg()`
#' using `cli::cli_alert_success()` to display the message.
#' - `msg_danger()`: To indicate a failed operation. Wrapper around `msg()`
#' using `cli::cli_alert_danger()` to display the message.
#' - `msg_warning()`: To indicate a warning. Wrapper around `msg_verbose()` using
#' `cli::cli_alert_warning()` to display the message.
#' - `msg_info()`: To provide additional information. Wrapper around `msg_verbose()`
#' using `cli::cli_alert_info()` to display the message.
#'
#' For more information on the verbosity levels, see `get_verbosity_level()`.
#'
#' @param message `character` string with the text to display.
#' @param levels_to_write `character` vector with the verbosity levels for
#' which the message should be displayed. Options are `minimal`, `verbose`, and
#' `debug`.
#' @param msg_fun The function to use for writing the message. Most commonly
#' from the cli package. Default is `cli::cli_alert_info()`.
#' @param ... Additional arguments to pass to `msg_fun()`
#' @param .envir The `environment` to use for evaluating the verbosity level.
#' Default `parent.frame()` will be sufficient for most use cases. Parsed on to
#' `msg_fun()`.
#'
#' @return Return from `msg_fun()`
#'
#' @examples
#' msg("General message")
#' msg_success("Operation successful")
#' msg_danger("Operation failed")
#' msg_warning("Warning message")
#' msg_info("Additional information")
#' @export
msg <- function(
    message,
    levels_to_write = c("minimal", "verbose", "debug"),
    msg_fun = cli::cli_alert_info,
    ...,
    .envir = parent.frame()) {
  levels_to_write <- rlang::arg_match(arg = levels_to_write, multiple = TRUE)

  if (!get_verbosity_level(env = .envir) %in% levels_to_write) {
    return(invisible())
  }

  msg_fun(message, ..., .envir = .envir)
}

#' @rdname msg
#' @export
msg_verbose <- function(
    message,
    msg_fun = cli::cli_alert_info,
    ...,
    .envir = parent.frame()) {
  msg(
    message,
    levels_to_write = c("verbose", "debug"),
    msg_fun = msg_fun,
    ...,
    .envir = .envir
  )
}

#' @rdname msg
#' @export
msg_debug <- function(
    message,
    msg_fun = cli::cli_alert_info,
    ...,
    .envir = parent.frame()) {
  msg(
    message,
    levels_to_write = "debug",
    msg_fun = msg_fun,
    ...,
    .envir = .envir
  )
}

#' @rdname msg
#' @export
msg_success <- function(
    message,
    ...,
    .envir = parent.frame()) {
  msg(
    message = message,
    msg_fun = cli::cli_alert_success,
    ...,
    .envir = .envir
  )
}

#' @rdname msg
#' @export
msg_danger <- function(
    message,
    ...,
    .envir = parent.frame()) {
  msg(
    message = message,
    msg_fun = cli::cli_alert_danger,
    ...,
    .envir = .envir
  )
}

#' @rdname msg
#' @export
msg_warning <- function(
    message,
    ...,
    .envir = parent.frame()) {
  msg_verbose(
    message = message,
    msg_fun = cli::cli_alert_warning,
    ...,
    .envir = .envir
  )
}

#' @rdname msg
#' @export
msg_info <- function(
    message,
    ...,
    .envir = parent.frame()) {
  msg_verbose(
    message = message,
    msg_fun = cli::cli_alert_info,
    ...,
    .envir = .envir
  )
}
