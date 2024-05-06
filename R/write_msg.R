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
#' # Use the `write_msg` function to give end user information depending on the
#' # verbosity level set in the package options. Fx. if such an option is set
#' # in a pckage called `callisto`, then `write_msg` can be used inside function
#' # definition in that package like so:
#' callisto::filter_with_popdata <- function(data, ...) {
#'  write_msg("Filtering {.field data} with {.field popdata}",
#'            levels_to_write = c("verbose", "debug"),
#'            msg_fun = cli::cli_h2)
#'
#'  dplyr::filter(data, ...)
#' }
#' }
#'
#' @export
write_msg <- function(message,
                      levels_to_write = c("verbose", "debug"),
                      msg_fun = cli::cli_alert_info,
                      ...,
                      opt_name = "verbosity_level",
                      global_opt_name = paste0("atmos.", opt_name)) { # TODO add a destination argument to control
  # whether to write to console, write to a file, etc.

  match.arg(levels_to_write,
            choices = c("quiet", "verbose", "debug"),
            several.ok = TRUE)

  # Is write_msg called within another function call?
  called_within <- length(sys.calls()) > 1

  # If it is called within another function call, get the options defined within
  # the namespace that function lives in and use msg_fun based on that option
  if (called_within) {
    ns_of_prev_fun <- ns_of_call(which = -2)
    # Get value of option
    verbosity_level <- get_opt(opt_name = opt_name,
                               global_opt_name = global_opt_name,
                               env = ns_of_prev_fun)

    if (verbosity_level %in% levels_to_write) {
      msg_fun(message, ...)
    }
  } else {
    # If write_msg is called by itelf, use msg_fun
    msg_fun(message, ...)
  }

  invisible()
}

#' Get the namespace of function in the call stack
#'
#' @description Get the namespace of the function somewhere in the call stack.
#' Default behavior will get the namespace of the package of the function that
#' called the function from which this is called. Used inside [write_msg()], so
#' the verbosity level set in the package, which uses [write_msg()], will be used
#'
#' @param which Passed onto [base::sys.call()]
#'
#' @return The namespace of the package that function in `sys.call(which)`
#' belongs to
#'
#' @examples
#' # Get the namespace of the function that this is in
#' zephyr:::ns_of_call()
#'
#' \dontrun{
#' # Get the namespace of package of the function that called this function
#' ns_of_call(which = -1)
#'
#' # Use case of write_msg function: Get the namespace of package of the
#' # function that called write_msg
#' write_msg <- function(...) {
#'  ...
#'  ns_of_call(which = -2)
#'  ...
#' }
#' }
#'
ns_of_call <- function(which = 0) {
  call_of_fun <- base::sys.call(which = which)

  if (rlang::is_call_simple(call_of_fun, ns = TRUE)) {
    # If the call is like pkgname::funname, get the namespace of pkg directly
    ns_name <- rlang::call_ns(call_of_fun)
    ns <- getNamespace(ns_name)
  } else {
    # If call is just funname, get the namespace of the package by looking for
    # it
    call_name <- rlang::call_name(call_of_fun)
    ns <- environment(get(call_name))
  }

  return(ns)
}
