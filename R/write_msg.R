#' Write messages based on verbosity level
#'
#' @description
#'
#' The `msg` function is a general function for writing messages to the console
#' based on options set using the [options] package. As a default, an option
#' called `verbosity_level` set in the package defining a function calling `msg`
#' is used. If a global option is set, it will overwrite the package level
#' option.
#'
#' @param message `character` of message to write
#' @param levels_to_write `character` vector of levels of verbosity for which
#' to display the message
#' @param msg_fun `function` taking `message` as first argument. Usually a
#' `cli_...` function
#' @param ... Additional arguments passed to `msg_fun`
#' @param opt_name `character` name of the option set by the [options] package.
#' Passed to [get_opt()]
#' @param global_opt_name `character` name of the global option which, if set,
#' will overwrite the package level option. Passed to [get_opt()]
#' @param which `integer` passed to [sys.function()]. Default is -1, meaning
#' `sys.function(-1)` will return the function that called `msg`
#' @param .envir `environment` passed to `msg_fun`
#'
#' @details
#' The `msg` function is a general function, which can be used to write messages
#' based on options.
#'
#' The `msg_debug` function is a wrapper around `msg` with
#' `levels_to_write = "debug"` and `msg_fun = cli::cli_inform`, while the
#' `msg_success` function is a wrapper around `msg` with
#' `levels_to_write = c("verbose", "debug")` and `msg_fun = cli::cli_alert_success`.
#'
#' @examples
#' \dontrun{
#' # Use the `msg` function to give end user information depending on the
#' # verbosity level set in the package options. Fx. if such an option is set
#' # in a package called `callisto` with `options::define_option("verbosity_level", ...)`,
#' # then `msg` can be used inside function definition in that package like so:
#' callisto::filter_with_popdata <- function(data, popfilter, ...) {
#'  msg("Filtering {.field data} with {.field {popfilter}}",
#'            levels_to_write = c("verbose", "debug"),
#'            msg_fun = cli::cli_h2)
#'
#'  msg_debug("Trying to filter data")
#'  dplyr::filter(data, ...)
#'  msg_success("Filtered data with filter {.field {popfilter}}")
#' }
#' }
#'
#' @export
msg <- function(message,
                levels_to_write = c("verbose", "debug"),
                msg_fun = cli::cli_alert_info,
                ...,
                opt_name = "verbosity_level",
                global_opt_name = paste0("atmos.", opt_name),
                which = -1,
                .envir = parent.frame()) { # TODO add a destination argument to control
  # whether to write to console, write to a file, etc.

  match.arg(levels_to_write,
            choices = c("quiet", "verbose", "debug"),
            several.ok = TRUE)

  # Is msg called within another function call?
  called_within <- length(sys.calls()) > 1

  # If msg is called within another function definition, get the options defined within
  # the namespace of that function. Then use msg_fun based on that option
  if (called_within) {
    # Get the namespace of the function that called msg
    ns_of_prev_fun <- environment(sys.function(which = which))
    # Get value of option
    verbosity_level <- get_opt(opt_name = opt_name,
                               global_opt_name = global_opt_name,
                               env = ns_of_prev_fun)

    if (verbosity_level %in% levels_to_write) {
      msg_fun(message, ..., .envir = .envir)
    }
  } else {
    # If msg is called by itelf, use msg_fun
    msg_fun(message, ...)
  }

  invisible()
}

#' @rdname msg
#' @export
msg_debug <- function(message,
                      ...,
                      opt_name = "verbosity_level",
                      global_opt_name = paste0("atmos.", opt_name),
                      .envir = parent.frame()) {
  msg(message,
      levels_to_write = "debug",
      msg_fun = cli::cli_inform,
      ...,
      opt_name = opt_name,
      global_opt_name = global_opt_name,
      which = -2,
      .envir = .envir)
}

#' @rdname msg
#' @export
msg_success <- function(message,
                        ...,
                        opt_name = "verbosity_level",
                        global_opt_name = paste0("atmos.", opt_name),
                        .envir = parent.frame()) {
  msg(message,
      levels_to_write = c("verbose", "debug"),
      msg_fun = cli::cli_alert_success,
      ...,
      opt_name = opt_name,
      global_opt_name = global_opt_name,
      which = -2,
      .envir = .envir)
}
