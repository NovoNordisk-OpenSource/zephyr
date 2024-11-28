#' Get `verbosity_level` option with priority hierarchy
#'
#' @description
#' This function retrieves the `verbosity_level` option using a priority hierarchy.
#'
#' @param env Environment to search for the package-specific option (default: parent.frame())
#'
#' @return Character string representing the verbosity level: "quiet", "minimal", "verbose", or "debug"
#' @export
#'
#' @details
#' The verbosity level is determined by checking the following sources in order:
#'
#' 1. **Package-specific global option**
#'    - Set with: `options(zephyr.verbosity_level = <level>)`
#'
#' 2. **General global option**
#'    - Set with: `options(verbosity_level = <level>)`
#'
#' 3. **Package-specific options**
#'    a. Internal package option
#'       - Set with: `define_option_pkg("verbosity_level", <level>)`
#'    b. Package-specific environment variable
#'       - Set with: `Sys.setenv(R_<PKG>_VERBOSITY_LEVEL = <level>)`
#'
#' 4. **ZEPHYR environment variable**
#'    - Set with: `Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = <level>)`
#'
#' 5. **Default value**
#'    - If no other option is set, defaults to "verbose"
#'
#' The function checks these options in the order listed and uses the first valid verbosity level it finds.
#'
#' Verbosity levels, from least to most verbose:
#' - "quiet": No messages
#' - "minimal": Only essential messages
#' - "verbose": Informative messages (default)
#' - "debug": Detailed messages for debugging
#'
#' This function is primarily used by the `msg` function family to determine
#' whether to display messages based on the current verbosity level.
#'
#' @examples
#' # Default behavior
#' get_verbosity_level()  # Returns "verbose"
#'
#' # Set a package-specific global option
#' options(zephyr.verbosity_level = "debug")
#' get_verbosity_level()  # Returns "debug"
#'
#' # Set a general global option
#' options(verbosity_level = "minimal")
#' get_verbosity_level()  # Still returns "debug" (package-specific option has priority)
#'
#' # Remove the package-specific global option
#' options(zephyr.verbosity_level = NULL)
#' get_verbosity_level()  # Now returns "minimal"
#'
#' # Set a package-specific option (assuming it's defined in the package)
#' define_option_pkg("verbosity_level", default = "quiet")
#' options(verbosity_level = NULL)  # Remove the general global option
#' get_verbosity_level()  # Returns "quiet"
#'
#' # Set the ZEPHYR environment variable
#' Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "verbose")
#' get_verbosity_level()  # Still returns "quiet" (package option has priority)
#'
#' # Remove all options and environment variable
#' options(zephyr.verbosity_level = NULL)
#' options(verbosity_level = NULL)
#' rm(list = ls(envir = .options))  # Remove package-specific options
#' Sys.unsetenv("R_ZEPHYR_VERBOSITY_LEVEL")
#' get_verbosity_level()  # Returns "verbose" (default)
#'
#' # Usage with msg functions
#' msg("This is a verbose message", levels_to_write = "verbose")
#' msg("This is a minimal message", levels_to_write = "minimal")
#' msg_debug("This is a debug message")
#' msg_success("This is a success message")
#' msg_minimal("This is a minimal message")
#'
get_verbosity_level <- function(env = parent.frame()) {
  # Check for a global option first
  global_option <- getOption("zephyr.verbosity_level")
  if (!is.null(global_option)) {
    return(validate_verbosity_level(global_option))
  }

  # Check for an environment-specific option
  env_specific_value <- getOption("verbosity_level")
  if (!is.null(env_specific_value)) {
    return(validate_verbosity_level(env_specific_value))
  }

  # Check for a package-specific option
  tryCatch({
    option_name <- get_package_option(option_name = "verbosity_level", pkg_env = env)
    pkg_specific_value <- opt_pkg(option_name, envir = env)
    if (!is.null(pkg_specific_value)) {
      return(validate_verbosity_level(pkg_specific_value))
    }
  }, error = function(e) {
    # If there's an error, we'll just move on to the next check
  })

  # Check for a Zephyr environment variable
  env_var <- Sys.getenv("R_ZEPHYR_VERBOSITY_LEVEL")
  if (nzchar(env_var)) {
    return(validate_verbosity_level(env_var))
  }

  # If no option is set, return the default value
  return("verbose")
}

#' Validate verbosity level
#'
#' @param level The verbosity level to validate
#' @return A valid verbosity level
#' @keywords internal
validate_verbosity_level <- function(level) {
  valid_levels <- c("quiet", "minimal", "verbose", "debug")
  level <- tolower(level)
  if (level %in% valid_levels) {
    return(level)
  } else {
    warning(sprintf("Invalid verbosity level: %s. Using default: verbose", level))
    return("verbose")
  }
}

#' Get Package Option
#'
#' This function searches for a specified option within a package environment.
#' It looks for objects that either end with the given option name or match it exactly.
#' The search is case-insensitive.
#'
#' @param option_name A character string specifying the name of the option to search for.
#' @param pkg_env The package environment to search in. This should be an environment object.
#'
#' @return The name of the matching option if found, otherwise NULL.
#'
#' @details
#' The function first searches in the main package environment. If the option is not found there,
#' it checks in the `.options` environment if it exists within the package environment.
#' If multiple matching options are found, it returns the first one and issues a warning.
#'
#' @note
#' This function does not return the value of the option, only its name.
#' To get the value, use `opt_pkg(result, envir = pkg_env)` on the returned result.
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_package' is loaded and has an option 'verbosity_level'
#' pkg_env <- asNamespace("my_package")
#' option_name <- get_package_option("verbosity_level", pkg_env)
#' if (!is.null(option_name)) {
#'   option_value <- opt_pkg(option_name, envir = pkg_env)
#'   print(paste("Value of", option_name, "is", option_value))
#' }
#' }
#'
#' @export
get_package_option <- function(option_name, pkg_env) {
  # Function to check in an environment for the option
  check_env <- function(env, opt_name) {
    all_objects <- ls(env)
    if (!is.null(all_objects)) {all_objects <- ls(env$.options)}
    # Match objects that either match option_name exactly or end with .option_name
    pattern <- paste0("^", opt_name, "$|\\.", opt_name, "$")
    matching_objects <- grep(pattern, all_objects, ignore.case = TRUE, value = TRUE)

    if (length(matching_objects) > 0) {
      if (length(matching_objects) > 1) {
        warning(paste("Multiple options found for", opt_name, ". Using the first one."))
        matching_objects <-  matching_objects[1]
      }
      return(matching_objects)
    }
    return(NULL)
  }


  # Check in the main environment
  result <- check_env(pkg_env, option_name)
  if (!is.null(result)) return(result)

  # If not found, check in the .options environment if it exists
  if (exists(".options", envir = pkg_env) && is.environment(pkg_env$.options)) {
    result <- check_env(pkg_env$.options, option_name)
    if (!is.null(result)) return(result)
  }

  warning(paste("No option found for", option_name, "in the package environment."))
  return(NULL)
}
