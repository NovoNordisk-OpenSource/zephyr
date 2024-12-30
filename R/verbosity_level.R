#' Get All Verbosity Levels
#'
#' This function retrieves all verbosity level settings from various sources,
#' including global options, environment variables, package-specific options,
#' package environments, and package-specific options using opt_pkg for all loaded packages.
#'
#' @return A list of verbosity level settings. Each element in the list contains:
#'   \item{env_name}{The name of the environment where the setting was found}
#'   \item{option_name}{The name of the option or variable}
#'   \item{value}{The value of the verbosity level setting}
#'   \item{source}{The source of the setting (e.g., "option", "env", "pkg_env", etc.)}
#'
#' @details
#' The function checks for verbosity settings in the following ways:
#' 1. Global options ending with ".verbosity_level" (case-insensitive)
#' 2. Environment variables of the form "R_*_VERBOSITY_LEVEL"
#' 3. The specific option "zephyr.verbosity_level"
#' 4. Package-specific options via the `opt_pkg` function
#' 5. Options in package environments
#' 6. Package-specific options using `opt_pkg` with package environments for all loaded packages
#'
#' @note This function relies on the `opt_pkg` function being available in the
#'   environment. If `opt_pkg` is not found, it will catch the error and continue
#'   with the other checks.
#'
#' @export
#'
#' @examples
#' # Set some options and environment variables for demonstration
#' options(package1.verbosity_level = "quiet", Package2.Verbosity_Level = "debug")
#' Sys.setenv(R_PACKAGE3_VERBOSITY_LEVEL = "info")
#'
#' # Get all verbosity levels
#' verbosity_levels <- get_all_verbosity_levels()
#'
#' # Print results
#' for (setting in verbosity_levels) {
#'   cat("Environment:", setting$env_name, "\n")
#'   cat("Option:", setting$option_name, "\n")
#'   cat("Value:", setting$value, "\n")
#'   cat("Source:", setting$source, "\n\n")
#' }
#'
#' # Clean up
#' options(package1.verbosity_level = NULL, Package2.Verbosity_Level = NULL)
#' Sys.unsetenv("R_PACKAGE3_VERBOSITY_LEVEL")
#'
#' @seealso \code{\link{get_verbosity_level}}
get_all_verbosity_levels <- function() {
  results <- list()

  # 1. Use getOption to search for options ending with .verbosity_level
  all_options <- options()
  verbosity_options <- grep(
    "\\.verbosity_level$",
    names(all_options),
    ignore.case = TRUE,
    value = TRUE
  )
  for (option in verbosity_options) {
    results <- c(results, list(list(
      env_name = "Global",
      option_name = option,
      value = getOption(option),
      source = "option"
    )))
  }

  # 2. Use Sys.getenv to search for option "R_*_VERBOSITY_LEVEL"
  env_vars <- Sys.getenv()
  verbosity_env_vars <- grep("^R_.*_VERBOSITY_LEVEL$", names(env_vars), value = TRUE)
  for (env_var in verbosity_env_vars) {
    results <- c(results, list(list(
      env_name = "Environment",
      option_name = env_var,
      value = Sys.getenv(env_var),
      source = "env"
    )))
  }

  # 3. Use getOption("zephyr.verbosity_level")
  zephyr_option <- getOption("zephyr.verbosity_level")
  if (!is.null(zephyr_option)) {
    results <- c(results, list(list(
      env_name = "Global",
      option_name = "zephyr.verbosity_level",
      value = zephyr_option,
      source = "zephyr_option"
    )))
  }

  # 4. Use opt_pkg to search for options ending with .verbosity_level and "R_*_VERBOSITY_LEVEL"
  tryCatch({
    pkg_options <- opt_pkg("verbosity_level")
    for (pkg in names(pkg_options)) {
      results <- c(results, list(list(
        env_name = paste0("package:", pkg),
        option_name = "verbosity_level",
        value = pkg_options[[pkg]],
        source = "pkg_option_1"
      )))
    }
  }, error = function(e) {
    message("Error when checking package-specific options: ", e$message)
  })

  # 5 and 6. Check for options in package environments and use opt_pkg with package environment
  loaded_packages <- search()
  package_envs <- grep("^package:", loaded_packages, value = TRUE)
  for (pkg_env in package_envs) {
    pkg_name <- sub("^package:", "", pkg_env)
    env <- as.environment(pkg_env)

    # 5. Check for options in package environments
    if (exists("verbosity_level", envir = env, inherits = FALSE)) {
      results <- c(results, list(list(
        env_name = pkg_env,
        option_name = "verbosity_level",
        value = get("verbosity_level", envir = env),
        source = "pkg_env_other_1"
      )))
    }

    # 6. Use opt_pkg with package environment
    tryCatch({
      pkg_opt <- opt_pkg("verbosity_level", envir = env)
      if (!is.null(pkg_opt)) {
        results <- c(results, list(list(
          env_name = pkg_env,
          option_name = "verbosity_level",
          value = pkg_opt,
          source = "pkg_env_other_2"
        )))
      }
    }, error = function(e) {
      message("Error when checking package-specific options with opt_pkg for ", pkg_name, ": ", e$message)
    })
  }

  return(results)
}


#' Get verbosity level with priority hierarchy
#'
#' @description
#' This function retrieves the `verbosity_level` option using a priority hierarchy.
#' It checks various sources for the verbosity level setting and returns the first
#' valid value it finds. While the examples use "zephyr", this function works with any package.
#'
#' @param env Environment to search for the package-specific option (default: parent.frame())
#'
#' @return Character string representing the verbosity level: "quiet", "minimal", "verbose", or "debug"
#'
#' @details
#' The function checks for the verbosity level in the following order:
#' 1. Global package-specific option (e.g., `options("packagename.verbosity_level")`)
#' 2. Package-specific environment variable (e.g., `R_PACKAGENAME_VERBOSITY_LEVEL`)
#' 3. Global zephyr package option (`options("zephyr.verbosity_level")`)
#' 4. Zephyr environment variable (`R_ZEPHYR_VERBOSITY_LEVEL`)
#' 5. Package-specific options using `opt_pkg("verbosity_level")`
#' 6. Default value ("verbose")
#'
#' Replace "package" with your actual package name in the above.
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
#' # Note: These examples use "zephyr" as the package name, but the function
#' # works with any package name. Replace "zephyr" with your package name in practice.
#'
#' # Default behavior
#' get_verbosity_level()  # Returns "verbose"
#'
#' # Using withr::with_envvar to temporarily set verbosity level
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::with_envvar(
#'     new = c("R_VERBOSITY_LEVEL" = "quiet"),
#'     code = print(get_verbosity_level())  # Prints "quiet"
#'   )
#' }
#'
#' # Verbosity level is back to default after with_envvar
#' get_verbosity_level()  # Returns "verbose"
#'
#' # Set a global package-specific option
#' options(zephyr.verbosity_level = "debug")
#' get_verbosity_level()  # Returns "debug"
#'
#' # Set a package-specific environment variable
#' Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "minimal")
#' get_verbosity_level()  # Returns "debug" (option has priority over env var)
#'
#' # Remove the global option to see the effect of the environment variable
#' options(zephyr.verbosity_level = NULL)
#' get_verbosity_level()  # Returns "minimal"
#'
#' # Reset to default
#' options(zephyr.verbosity_level = NULL)
#' Sys.unsetenv("R_ZEPHYR_VERBOSITY_LEVEL")
#' get_verbosity_level()  # Returns "verbose" (default)
#'
#' @seealso
#' \code{\link{msg}}
get_verbosity_level <-  function(env = parent.frame()) {
  # TODO: Assert verbosity values
  # Get all verbosity levels
  all_levels <- get_all_verbosity_levels()

  # Helper function to get package name
  get_package_name <-  function(env = environment(), which = -1) {
    # Helper function to search for the most immediate package environment
    search_for_immediate_package <- function(e) {
      if (isNamespace(e)) {
        return(list(name = getNamespaceName(e), depth = 0))
      }

      # Check if the environment has a 'name' attribute
      env_name <- attr(e, "name")
      if (!is.null(env_name) && env_name != "R_GlobalEnv") {
        return(list(name = env_name, depth = 0))
      }

      result <- list(name = NULL, depth = Inf)

      # Check all objects in the environment
      for (obj_name in ls(envir = e, all.names = TRUE)) {
        obj <- try(get(obj_name, envir = e), silent = TRUE)
        if (inherits(obj, "try-error")) next
        if (is.environment(obj)) {
          # Check if this environment is a package namespace or has a name attribute
          if (isNamespace(obj)) {
            return(list(name = getNamespaceName(obj), depth = 1))
          }
          obj_name <- attr(obj, "name")
          if (!is.null(obj_name) && obj_name != "R_GlobalEnv") {
            return(list(name = obj_name, depth = 1))
          }
          # Recursively search this environment
          sub_result <- search_for_immediate_package(obj)
          if (!is.null(sub_result$name) && sub_result$depth + 1 < result$depth) {
            result$name <- sub_result$name
            result$depth <- sub_result$depth + 1
          }
        }
      }

      return(result)
    }

    # Helper function to remove "package:" prefix
    remove_package_prefix <- function(name) {
      if (!is.null(name) && startsWith(name, "package:")) {
        return(sub("^package:", "", name))
      }
      return(name)
    }

    # Main function logic
    if (is.character(env)) {
      # Remove quotes if present
      env <- gsub("^[\"']|[\"']$", "", env)

      # If it starts with "package:", assume it's a package name
      if (startsWith(env, "package:")) {
        return(remove_package_prefix(env))
      }

      # If not an existing environment, assume it's a package name
      if (!exists(env, mode = "environment")) {
        return(env)
      }

      # If it's a character string referring to an environment, get that environment
      env <- try(as.environment(env), silent = TRUE)
      if (inherits(env, "try-error")) {
        return(NULL)
      }
    }

    if (is.environment(env)) {
      # Check if it's a namespace
      if (isNamespace(env)) {
        return(getNamespaceName(env))
      }

      # Check for name attribute
      env_name <- attr(env, "name")
      if (!is.null(env_name) && env_name != "R_GlobalEnv") {
        return(remove_package_prefix(env_name))
      }

      # Search for the most immediate package environment
      result <- search_for_immediate_package(env)
      if (!is.null(result$name)) {
        return(remove_package_prefix(result$name))
      }
    }

    # Check if we're in a package directory
    if (file.exists("DESCRIPTION")) {
      desc <- read.dcf("DESCRIPTION")
      if ("Package" %in% colnames(desc)) {
        return(desc[1, "Package"])
      }
    }

    # If not in a package environment, try to find the package name from calls
    calls <- sys.calls()
    for (call in calls) {
      if (identical(call[[1]], as.name("::")) || identical(call[[1]], as.name(":::"))) {
        return(as.character(call[[2]]))
      }
    }

    # Check the calling environment
    called_within <- length(sys.calls()) > 1
    if (called_within) {
      ns_of_fun <- tryCatch({
        environment(sys.function(which = which))
      }, error = function(e) {
        parent.frame(which)
      })

      if (isNamespace(ns_of_fun)) {
        return(getNamespaceName(ns_of_fun))
      }

      env_name <- attr(ns_of_fun, "name")
      if (!is.null(env_name) && env_name != "R_GlobalEnv") {
        return(remove_package_prefix(env_name))
      }
    }

    # Check if we're in a directory that matches a loaded package
    current_dir <- basename(getwd())
    if (current_dir %in% loadedNamespaces()) {
      return(current_dir)
    }

    return(NULL)
  }
  pkg_name <- get_package_name(env)

  # 1. Global package-specific option
  if (!is.null(pkg_name)) {
    pkg_specific <- Filter(function(x) x$env_name == "Global" && x$option_name == paste0(pkg_name, ".verbosity_level"), all_levels)
    if (length(pkg_specific) > 0) {
      return(validate_verbosity_level(pkg_specific[[1]]$value))
    }
  }

  # 2. Package-specific environment variable
  if (!is.null(pkg_name)) {
    pkg_env_specific <-  Filter(function(x) x$env_name == "Environment" && x$option_name == paste0("R_", toupper(pkg_name), "_VERBOSITY_LEVEL"), all_levels)
    if (length(pkg_env_specific) > 0) {
      return(validate_verbosity_level(pkg_env_specific[[1]]$value))
    }
  }

  # 3. Global zephyr package option
  zephyr_option <-  Filter(function(x) x$env_name == "Global" && x$option_name == "zephyr.verbosity_level", all_levels)
  if (length(zephyr_option) > 0) {
    return(validate_verbosity_level(zephyr_option[[1]]$value))
  }

  # 4. Zephyr environment variable
  zephyr_env <-  Filter(function(x) x$env_name == "Environment" && x$option_name == "R_ZEPHYR_VERBOSITY_LEVEL", all_levels)
  if (length(zephyr_env) > 0) {
    return(validate_verbosity_level(zephyr_env[[1]]$value))
  }

  # 5. Package-specific options
  if (!is.null(pkg_name)) {
    pkg_specific_opt <-  Filter(function(x) x$env_name == paste0("package:", pkg_name) && x$source %in% c("pkg_option_1", "pkg_env_other_1", "pkg_env_other_2"), all_levels)
    if (length(pkg_specific_opt) > 0) {
      return(validate_verbosity_level(pkg_specific_opt[[1]]$value))
    }
  }

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
