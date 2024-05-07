#' Write messages based on verbosity level
#'
#' @description
#'
#' Write based on verbosity level set as option in package
#'
#' @param message `character` of message to write
#' @param levels_to_write `character` vector of levels of verbosity for which
#' to display the message
#' @param msg_fun `function` taking `message` as first argument. Usually a
#' `cli_...` function
#' @param opt_name `character` name of the option. Passed to [get_opt()]
#' @param global_opt_name `character` name of the global option which, if set,
#' will overwrite the package level option. Passed to [get_opt()]
#' @param ... Additional arguments passed to `msg_fun`
#'
#'
#' @examples
#' \dontrun{
#' # Use the `msg` function to give end user information depending on the
#' # verbosity level set in the package options. Fx. if such an option is set
#' # in a pckage called `callisto`, then `msg` can be used inside function
#' # definition in that package like so:
#' callisto::filter_with_popdata <- function(data, ...) {
#'  msg("Filtering {.field data} with {.field popdata}",
#'            levels_to_write = c("verbose", "debug"),
#'            msg_fun = cli::cli_h2)
#'
#'  dplyr::filter(data, ...)
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

#' @export
msg_debug <- function(message,
                      ...,
                      msg_fun = cli::cli_inform,
                      opt_name = "verbosity_level",
                      global_opt_name = paste0("atmos.", opt_name)) {
  msg(message,
      levels_to_write = "debug",
      msg_fun = msg_fun,
      ...,
      opt_name = opt_name,
      global_opt_name = global_opt_name,
      which = -2,
      .envir = parent.frame())
}

#' @export
msg_success <- function(message,
                        ...,
                        msg_fun = cli::cli_alert_success,
                        opt_name = "verbosity_level",
                        global_opt_name = paste0("atmos.", opt_name)) {
  msg(message,
      levels_to_write = c("verbose", "debug"),
      msg_fun = msg_fun,
      ...,
      opt_name = opt_name,
      global_opt_name = global_opt_name,
      which = -2,
      .envir = parent.frame())
}
