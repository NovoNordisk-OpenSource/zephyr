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
#' get_verbosity_level()  # Returns "verbose" (default)
#'
#' @seealso
#' \code{\link{msg}}, \code{\link{msg_debug}}, \code{\link{msg_success}}, \code{\link{msg_minimal}}
#'
#' @importFrom utils getFromNamespace
get_verbosity_level <-  function(env = parent.frame()) {
  # Helper function to get package name
  get_package_name <- function() {
    env_name <- environmentName(env)
    if (env_name != "") {
      return(sub("package:", "", env_name))
    }
    # If not in a package environment, try to find the package name
    calls <- sys.calls()
    for (call in calls) {
      if (identical(call[[1]], as.name("::")) || identical(call[[1]], as.name(":::"))) {
        return(as.character(call[[2]]))
      }
    }
    return(NULL)
  }

  pkg_name <- get_package_name()

  # 1. Global package-specific option
  if (!is.null(pkg_name)) {
    pkg_specific <- getOption(paste0(pkg_name, ".verbosity_level"))
    if (!is.null(pkg_specific)) {
      return(validate_verbosity_level(pkg_specific))
    }
  }

  # 2. Package-specific environment variable
  if (!is.null(pkg_name)) {
    pkg_env_specific <- Sys.getenv(paste0("R_", toupper(pkg_name), "_VERBOSITY_LEVEL"))
    if (nzchar(pkg_env_specific)) {
      return(validate_verbosity_level(pkg_env_specific))
    }
  }

  # 3. Global zephyr package options
  global_option <- getOption("zephyr.verbosity_level")
  if (!is.null(global_option)) {
    return(validate_verbosity_level(global_option))
  }

  # 4. Zephyr environment variables
  env_var <- Sys.getenv("R_ZEPHYR_VERBOSITY_LEVEL")
  if (nzchar(env_var)) {
    return(validate_verbosity_level(env_var))
  }

  # 5. Package-specific options
  tryCatch({
    pkg_specific_value <- opt_pkg("verbosity_level", envir = env)
    if (!is.null(pkg_specific_value)) {
      return(validate_verbosity_level(pkg_specific_value))
    }
  }, error = function(e) {
    message("Error when checking package-specific option: ", e$message)
  })

  # 6. Default value
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

#' Get Package Name and Verbosity Level
#'
#' This function determines the calling package name and checks for a corresponding
#' verbosity level option. It works in package environments, devtools environments,
#' and the global environment.
#'
#' @return A list with two elements:
#'   \item{package_name}{A character string with the detected package name, or NULL if in global environment}
#'   \item{verbosity_level}{A character string with the verbosity level, or NULL if not set}
#'
#' @examples
#' result <-  get_package_name_and_verbosity()
#' print(result)
#'
#' @export
get_package_name_and_verbosity <- function() {
  result <- list(package_name = NULL, verbosity_level = NULL)
  valid_levels <- c("quiet", "minimal", "verbose", "debug")

  # Function to get the calling package name
  get_calling_pkg_name <- function() {
    # Get the call stack
    calls <- sys.calls()

    # Iterate through the call stack to find the calling package
    for (i in seq_along(calls)) {
      # Get the environment of the call
      env <- parent.frame(n = i)

      # Check if this environment is a namespace
      if (isNamespace(env)) {
        pkg_name <- getNamespaceName(env)
        if (pkg_name != "zephyr") {
          return(pkg_name)
        }
      }

      # Check for devtools environment
      if (identical(env, .GlobalEnv) && "devtools_shims" %in% search()) {
        # We're likely in a devtools context
        # Look for the package environment in the search path
        search_path <- search()
        pkg_env <- grep("^package:", search_path, value = TRUE)
        if (length(pkg_env) > 0) {
          return(sub("^package:", "", pkg_env[1]))
        }
      }
    }

    # If we couldn't find a non-zephyr namespace, return NULL
    return(NULL)
  }

  # Get the calling package name
  result$package_name <-  get_calling_pkg_name()

  if (is.null(result$package_name)) {
    message("Function called from global environment or unable to determine package name.")
  } else {
    # Check for package-specific verbosity level option
    option_name <- paste0(result$package_name, ".verbosity_level")
    verbosity_value <- getOption(option_name)

    if (!is.null(verbosity_value) && verbosity_value %in% valid_levels) {
      result$verbosity_level <- verbosity_value
    }
  }

  # If no package-specific verbosity level is set, check for general verbosity_level option
  if (is.null(result$verbosity_level)) {
    general_verbosity <- getOption("verbosity_level")
    if (!is.null(general_verbosity) && general_verbosity %in% valid_levels) {
      result$verbosity_level <- general_verbosity
    }
  }

  return(result)
}

#' Get Package Name and Environment Variable Verbosity Level
#'
#' This function determines the calling package name and checks for a corresponding
#' environment variable that sets the verbosity level.
#'
#' @return A list with three elements:
#'   \item{package_name}{A character string with the detected package name, or NULL if in global environment}
#'   \item{env_var_name}{A character string with the name of the environment variable}
#'   \item{verbosity_level}{A character string with the verbosity level, or NULL if not set or invalid}
#'
#' @examples
#' result <-  get_package_name_and_env_verbosity()
#' print(result)
#'
#' @export
get_package_name_and_env_verbosity <- function() {
  # Initialize result
  result <- list(package_name = NULL, env_var_name = NULL, verbosity_level = NULL)

  # Valid verbosity levels
  valid_levels <- c("quiet", "minimal", "verbose", "debug")

  # Function to get the calling package name
  get_calling_pkg_name <- function() {
    # Get the call stack
    calls <- sys.calls()

    # Iterate through the call stack to find the calling package
    for (i in seq_along(calls)) {
      # Get the environment of the call
      env <- parent.frame(n = i)

      # Check if this environment is a namespace
      if (isNamespace(env)) {
        pkg_name <- getNamespaceName(env)
        if (pkg_name != "zephyr") {
          return(pkg_name)
        }
      }

      # Check for devtools environment
      if (identical(env, .GlobalEnv) && "devtools_shims" %in% search()) {
        # We're likely in a devtools context
        # Look for the package environment in the search path
        search_path <- search()
        pkg_env <- grep("^package:", search_path, value = TRUE)
        if (length(pkg_env) > 0) {
          return(sub("^package:", "", pkg_env[1]))
        }
      }
    }

    # If we couldn't find a non-zephyr namespace, return NULL
    return(NULL)
  }

  # Get the current package name
  result$package_name <-  get_calling_pkg_name()

  if (is.null(result$package_name)) {
    message("Function called from global environment or unable to determine package name.")
    return(result)
  }

  # Construct the environment variable name
  result$env_var_name <- paste0("R_", toupper(result$package_name), "_VERBOSITY_LEVEL")

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
