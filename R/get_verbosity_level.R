
#' Get `verbosity_level` option using the `options` package, allowing a global option
#'
#' @description
#' Get option using the `options` package, where the
#' function allows a global option to overwrite individual package options.
#'
#' @param env Environment to get the option from (set with the `options` package)
#'
#' @return Value of the option
#' @export
#'
#' @examples
#' # Set a package-level verbosity option
#' define_option_pkg("verbosity_level", default = "verbose",
#'                   desc = "Set the verbosity level for messages")
#'
#' # Get the verbosity level (should return "verbose")
#' get_verbosity_level()
#'
#' # Set a global option
#' options(zephyr.verbosity_level = "debug")
#'
#' # Get the verbosity level (should now return "debug")
#' get_verbosity_level()
#'
#' # Set an environment variable
#' Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "quiet")
#'
#' # Get the verbosity level (should still return "debug" due to option priority)
#' get_verbosity_level()
#'
#' # Remove the global option
#' options(zephyr.verbosity_level = NULL)
#'
#' # Get the verbosity level (should now return "quiet" from the environment variable)
#' get_verbosity_level()
get_verbosity_level <-  function(env = parent.frame()) {
  # Check for a global option first
  global_option <- getOption("zephyr.verbosity_level")
  if (!is.null(global_option)) {
    return(global_option)
  }

  # Check for an environment-specific option
  tryCatch({
    env_specific_value <- opt_pkg("verbosity_level", envir = env)
    if (!is.null(env_specific_value)) {
      return(env_specific_value)
    }
  }, error = function(e) {
    # If there's an error, we'll just move on to the next check
  })

  # Check for a package-level option
  tryCatch({
    pkg_spec <- get_option_spec_pkg("verbosity_level", envir = env, print_spec = FALSE)
    if (!is.null(pkg_spec)) {
      pkg_value <- opt_pkg("verbosity_level", envir = env)
      if (!is.null(pkg_value)) {
        return(pkg_value)
      }
    }
  }, error = function(e) {
    # If there's an error, we'll just move on to the next check
  })

  # Check for an environment variable
  pkg_env_var <- Sys.getenv("R_PKG_ENV_VERBOSITY_LEVEL")
  if (nzchar(pkg_env_var)) {
    return(pkg_env_var)
  }

  env_var <- Sys.getenv("R_ZEPHYR_VERBOSITY_LEVEL")
  if (nzchar(env_var)) {
    return(env_var)
  }

  # If no option is set, return the default value
  return("verbose")
}
