#' Get `verbosity_level` option, prioritizing different sources.
#'
#' This function retrieves the value of the `verbosity_level` option, prioritizing
#' different sources according to the following rules:
#'
#' 1. If the option is not defined for the specified environment, use the option
#'    set on the "zephyr" level (using the default if nothing is done).
#' 2. If the option is set on the "zephyr" level, use that one.
#' 3. If the option is set on the "env" level, use that one.
#' 4. If the option is not set as an option on the package level, and the "zephyr"
#'    option is set using an environment variable, use that one.
#' 5. If the "zephyr" level option is not set, use the option set on the package
#'    level, whether default or environment variable.
#'
#' @param env Environment to get the option from.
#'
#' @return Value of the option.
#' @export
#'
#' @examples
#' # Define the option in the "zephyr" namespace
#'
#' # Get the verbosity level in the current environment
#' get_verbosity_level()
#'
#' # Get the verbosity level in a specific environment
#' env <- new.env()
#' get_verbosity_level(env)
get_verbosity_level <- function(env = parent.frame()) {

  # Get the "zephyr" level option and its source
  zephyr_verbosity_level_source <- opt_source_pkg("verbosity_level", env = getNamespace("zephyr"))
  zephyr_verbosity_level <- opt_pkg("verbosity_level", env = getNamespace("zephyr"))

  # 1. If option is not defined for the specified environment, use the "zephyr" level option
  if (is.null(attr(env$.options, "spec"))) {
    return(zephyr_verbosity_level)
  }

  # Get the package level option and its source
  pkg_verbosity_level_source <- opt_source_pkg("verbosity_level", env = env)
  pkg_verbosity_level <- opt_pkg("verbosity_level", env = env)

  # 2. If option is set on "zephyr level" use that one
  if (zephyr_verbosity_level_source == "option") {
    return(zephyr_verbosity_level)
  }

  # 3. If option is set on "env level" use that one
  if (pkg_verbosity_level_source == "option") {
    return(pkg_verbosity_level)
  }

  # 4. If option is not set as an option on package level, and "zephyr" option is set
  # using environment variable, use that one
  if (zephyr_verbosity_level_source == get_envname_options()) {
    return(zephyr_verbosity_level)
  }

  # 5. If "zephyr" level option is not set, use the option set on package level
  return(pkg_verbosity_level)
}
