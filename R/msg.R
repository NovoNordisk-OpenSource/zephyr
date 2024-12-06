#' Write messages based on verbosity level
#'
#' @description
#' The `msg` function is a general function for writing messages to the console
#' based on options set using the [options] package. As a default, an option
#' called `verbosity_level` set in the package defining a function calling `msg`
#' is used. If a global option with prefix `zephyr.` is set, it will overwrite
#' the package level option.
#'
#' Valid values are `quiet`, `minimal`, `verbose`, and `debug`.
#' - `quiet`: No messages are displayed
#' - `minimal`: Only essential messages are displayed
#' - `verbose`: Informative messages are displayed (default)
#' - `debug`: Detailed messages for debugging are displayed
#'
#' @param message `character` of message to write
#' @param levels_to_write `character` vector of levels of verbosity for which
#' to display the message. Valid values are `quiet`, `minimal`, `verbose`, and `debug`
#' @param msg_fun `function` taking `message` as first argument. Usually a
#' `cli_...` function
#' @param ... Additional arguments passed to `msg_fun`
#' @param verbosity_level The verbosity level to use. If `NULL`, the function
#' will use the `which` argument to determine the environment in which to find
#' an option called `verbosity_level`. By default, it will look in the environment
#' of the function calling `msg(_...)`. If no option is set in this calling
#' environment, it will look in the `zephyr` namespace.
#' @param which `integer` passed to [sys.function()] in case `verbosity_level = NULL`.
#' Default is -1, meaning it will look in the environment of the function calling `msg(_...)`.
#' @param .envir `environment` passed to `msg_fun`
#'
#' @details
#' The `msg` function is a general function, which can be used to write messages
#' based on options.
#'
#' The `msg_debug` function is a wrapper around `msg` with
#' `levels_to_write = "debug"` and `msg_fun = cli::cli_inform`.
#'
#' The `msg_success` function is a wrapper around `msg` with
#' `levels_to_write = c("minimal", "verbose", "debug")` and `msg_fun = cli::cli_alert_success`.
#'
#' The `msg_minimal` function is a wrapper around `msg` with
#' `levels_to_write = c("minimal", "verbose", "debug")` and `msg_fun = cli::cli_alert_info`.
#'
#' @return None
#'
#' @examples
#' filter_data <- function(data, infilter, ...) {
#'   infilter_e <- rlang::enquo(infilter)
#'   infilter_lb <- rlang::as_label(infilter_e)
#'
#'   msg(
#'     "Attempting to filter {.field data} by {.field {infilter_lb}}",
#'     levels_to_write = c("verbose", "debug"),
#'     msg_fun = cli::cli_h2
#'   )
#'
#'   msg_debug("debug: Trying to filter data")
#'
#'   result <- data |>
#'     dplyr::filter({{infilter}})
#'
#'   msg_success("success: Data filtered by {.field {infilter_lb}}")
#'   msg_minimal("minimal: Filtered {nrow(result)} rows")
#'
#'   head(result)
#' }
#'
#' # Test with different verbosity levels
#' withr::with_options(list(verbosity_level = "quiet"),
#'   filter_data(data = cars, infilter = speed > 12))
#'
#' withr::with_options(list(verbosity_level = "minimal"),
#'   filter_data(data = cars, infilter = speed > 12))
#'
#' withr::with_options(list(verbosity_level = "verbose"),
#'   filter_data(data = cars, infilter = speed > 12))
#'
#' withr::with_options(list(verbosity_level = "debug"),
#'   filter_data(data = cars, infilter = speed > 12))
#'
#' @export
msg <-  function(message,
  levels_to_write = c("minimal", "verbose", "debug"),
  msg_fun = cli::cli_alert_info,
  ...,
  verbosity_level = NULL,
  which = -1,
  .envir = parent.frame()) {

  match.arg(levels_to_write,
    choices = c("quiet", "minimal", "verbose", "debug"),
    several.ok = TRUE)

  if (is.null(verbosity_level)) {
    called_within <- length(sys.calls()) > 1

    if (called_within) {
      ns_of_fun <- tryCatch({
        environment(sys.function(which = which))
      }, error = function(e) {
        parent.frame()
      })
    } else {
      ns_of_fun <- parent.frame()
    }
    verbosity_level <- get_verbosity_level(env = ns_of_fun)
  }

  if (verbosity_level %in% levels_to_write) {
    msg_fun(message, ..., .envir = .envir)
  }

  invisible()
}

#' @rdname msg
#' @export
msg_debug <- function(message,
  ...,
  verbosity_level = NULL,
  which = -1,
  .envir = parent.frame()) {
  msg(message,
    levels_to_write = "debug",
    msg_fun = cli::cli_inform,
    ...,
    verbosity_level = verbosity_level,
    which = -1 + which,
    .envir = .envir)
}

#' @rdname msg
#' @export
msg_success <- function(message,
  ...,
  verbosity_level = NULL,
  which = -1,
  .envir = parent.frame()) {
  msg(message,
    levels_to_write = c("minimal", "verbose", "debug"),
    msg_fun = cli::cli_alert_success,
    ...,
    verbosity_level = verbosity_level,
    which = -1 + which,
    .envir = .envir)
}

#' @rdname msg
#' @export
msg_minimal <- function(message,
  ...,
  verbosity_level = NULL,
  which = -1,
  .envir = parent.frame()) {
  msg(message,
    levels_to_write = c("minimal", "verbose", "debug"),
    msg_fun = cli::cli_alert_info,
    ...,
    verbosity_level = verbosity_level,
    which = -1 + which,
    .envir = .envir)
}
