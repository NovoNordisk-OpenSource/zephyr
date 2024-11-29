#' Get verbosity level with priority hierarchy
#'
#' @description
#' This function retrieves the `verbosity_level` option using a priority hierarchy.
#' It checks various sources for the verbosity level setting and returns the first
#' valid value it finds.
#'
#' @param env Environment to search for the package-specific option (default: parent.frame())
#'
#' @return Character string representing the verbosity level: "quiet", "minimal", "verbose", or "debug"
#'
#' @details
#' The function checks the following sources in order of priority:
#' 1. Global package-specific option: `options("[package].verbosity_level")`
#' 2. Package-specific environment variable: `Sys.getenv("R_[PACKAGE]_VERBOSITY_LEVEL")`
#' 3. Global Zephyr package option: `options("zephyr.verbosity_level")`
#' 4. Zephyr environment variable: `Sys.getenv("R_ZEPHYR_VERBOSITY_LEVEL")`
#' 5. Package-specific option set with `define_option_pkg("verbosity_level")`
#' 6. Default value: "verbose"
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
#' @export
#'
#' @examples
#' # Default behavior
#' get_verbosity_level()  # Returns "verbose"
#'
#' # Set a package-specific global option
#' options(mypackage.verbosity_level = "debug")
#' get_verbosity_level()  # Returns "debug"
#'
#' # Set a package-specific environment variable
#' Sys.setenv(R_MYPACKAGE_VERBOSITY_LEVEL = "minimal")
#' get_verbosity_level()  # Returns "debug" (option has priority over env var)
#'
#' # Set Zephyr global option
#' options(zephyr.verbosity_level = "quiet")
#' options(mypackage.verbosity_level = NULL)
#' Sys.unsetenv("R_MYPACKAGE_VERBOSITY_LEVEL")
#' get_verbosity_level()  # Returns "quiet"
#'
#' # Set Zephyr environment variable
#' Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "minimal")
#' options(zephyr.verbosity_level = NULL)
#' get_verbosity_level()  # Returns "minimal"
#'
#' # Set package-specific option
#' define_option_pkg("verbosity_level", default = "debug", envir = parent.frame())
#' Sys.unsetenv("R_ZEPHYR_VERBOSITY_LEVEL")
#' get_verbosity_level()  # Returns "debug"
#'
#' # Reset to default
#' options(mypackage.verbosity_level = NULL)
#' Sys.unsetenv("R_MYPACKAGE_VERBOSITY_LEVEL")
#' options(zephyr.verbosity_level = NULL)
#' Sys.unsetenv("R_ZEPHYR_VERBOSITY_LEVEL")
#' rm(list = ls(envir = .options))  # Remove package-specific options
#' get_verbosity_level()  # Returns "verbose" (default)
#'
#' @seealso
#' \code{\link{msg}}, \code{\link{msg_debug}}, \code{\link{msg_success}}, \code{\link{msg_minimal}}
#'
#' @importFrom utils getFromNamespace
get_verbosity_level <- function(env = parent.frame()) {
  # 1. **global package option**
  #    options("[package].verbosity_level")
  if (!is.null(get_package_name_and_verbosity()$verbosity_level)) {
    return(get_package_name_and_verbosity()$verbosity_level)
  }

  # 2. **package environment variables**
  #    Sys.getenv("R_WHIRL_VERBOSITY_LEVEL")
  if (!is.null(get_package_name_and_verbosity()$verbosity_level)) {
    return(get_package_name_and_env_verbosity()$verbosity_level)
  }

  # 3. **global zephyr package options**
  #     options("zephyr.verbosity_level")
  global_option <- getOption("zephyr.verbosity_level")
  if (!is.null(global_option)) {
    return(validate_verbosity_level(global_option))
  }

  # 4. **zephyr environment variables**
  #     Sys.getenv("R_ZEPHYR_VERBOSITY_LEVEL")
  env_var <- Sys.getenv("R_ZEPHYR_VERBOSITY_LEVEL")
  if (nzchar(env_var)) {
    return(validate_verbosity_level(env_var))
  }

  # 5. **Package-specific options**
  #   Default set with define_option_pkg("verbosity_level")
  # Check for an environment-specific option

  tryCatch({
    option_name <- get_package_option(option_name = "verbosity_level", pkg_env = env)
    pkg_specific_value <- opt_pkg(option_name, envir = env)
    if (!is.null(pkg_specific_value)) {
      return(validate_verbosity_level(pkg_specific_value))
    }
  }, error = function(e) {
    # If there's an error, we'll just move on to set a default
  })
  # 6. **Default value**
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

#' Get Package Name and Verbosity Level from Global Options
#'
#' This function retrieves the current package name and searches the global environment
#' for an option that specifies the verbosity level for this package.
#'
#' @return A list with two elements:
#'   \item{package_name}{A character string containing the name of the current package.}
#'   \item{verbosity_level}{A character string indicating the verbosity level,
#'     or NULL if not specified or if invalid. Possible values are "quiet", "minimal", "verbose", and "debug".}
#'
#' @details
#' The function determines the current package name, then looks for a global option
#' named "[package_name].verbosity_level". If found, it returns the package name and verbosity level.
#' The verbosity level is NULL if not specified or if invalid.
#'
#' @examples
#' # Assuming this function is called from within the "zephyr" package:
#' # Set a global option for the current package with verbose verbosity level
#' options(zephyr.verbosity_level = "verbose")
#' get_package_name_and_verbosity()  # Returns list(package_name = "zephyr", verbosity_level = "verbose")
#'
#' @export
get_package_name_and_verbosity <- function() {
  # Initialize result
  result <- list(package_name = NULL, verbosity_level = NULL)

  # Valid verbosity levels
  valid_levels <- c("quiet", "minimal", "verbose", "debug")

  # Get the current package name
  current_package <- sub("^package:", "", environmentName(parent.env(globalenv())))

  if (current_package == "R_GlobalEnv") {
    stop("This function must be called from within a package environment.")
  }

  result$package_name <- current_package

  # Construct the option name
  option_name <- paste0(current_package, ".verbosity_level")

  # Get the verbosity level
  verbosity_value <- getOption(option_name)

  # Check if the verbosity level is valid
  if (!is.null(verbosity_value)) {
    if (verbosity_value %in% valid_levels) {
      result$verbosity_level <- verbosity_value
    } else {
      warning("Invalid verbosity level. Setting to NULL.")
      result$verbosity_level <- NULL
    }
  }

  return(result)
}

#' Get Package Name, Environment Variable Name, and Verbosity Level
#'
#' This function retrieves the current package name, constructs the environment variable name,
#' and checks the environment variable R_[PACKAGE]_VERBOSITY_LEVEL for the verbosity level of this package.
#'
#' @return A list with three elements:
#'   \item{package_name}{A character string containing the name of the current package.}
#'   \item{env_var_name}{A character string containing the name of the environment variable checked.}
#'   \item{verbosity_level}{A character string indicating the verbosity level,
#'     or NULL if not specified or if invalid. Possible values are "quiet", "minimal", "verbose", and "debug".}
#'
#' @details
#' The function determines the current package name, constructs the environment variable name,
#' then looks for an environment variable named R_[PACKAGE]_VERBOSITY_LEVEL.
#' If found and valid, it returns the package name, environment variable name, and verbosity level.
#' The verbosity level is NULL if not specified or if invalid.
#'
#' @examples
#' # Assuming this function is called from within the "zephyr" package:
#' # Set an environment variable for the current package with verbose verbosity level
#' Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "verbose")
#' get_package_name_and_env_verbosity()
#' # Returns list(package_name = "zephyr", env_var_name = "R_ZEPHYR_VERBOSITY_LEVEL", verbosity_level = "verbose")
#'
#' @export
get_package_name_and_env_verbosity <- function() {
  # Initialize result
  result <- list(package_name = NULL, env_var_name = NULL, verbosity_level = NULL)

  # Valid verbosity levels
  valid_levels <- c("quiet", "minimal", "verbose", "debug")

  # Get the current package name
  current_package <- sub("^package:", "", environmentName(parent.env(globalenv())))

  if (current_package == "R_GlobalEnv") {
    stop("This function must be called from within a package environment.")
  }

  result$package_name <- current_package

  # Construct the environment variable name
  result$env_var_name <- paste0("R_", toupper(current_package), "_VERBOSITY_LEVEL")

  # Get the verbosity level from the environment variable
  verbosity_value <- Sys.getenv(result$env_var_name, unset = NA)

  # Check if the verbosity level is set and valid
  if (!is.na(verbosity_value)) {
    if (verbosity_value %in% valid_levels) {
      result$verbosity_level <- verbosity_value
    } else {
      warning("Invalid verbosity level in environment variable. Setting to NULL.")
      result$verbosity_level <- NULL
    }
  }

  return(result)
}
