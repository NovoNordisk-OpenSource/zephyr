#' Create an option specification
#'
#' This function creates an option specification with various parameters.
#'
#' @param name The name of the option.
#' @param default The default value of the option.
#' @param desc A description of the option.
#' @param option_name The name of the R option (defaults to `name`).
#' @param envvar_name The name of the environment variable (defaults to `R_[uppercase name]`).
#' @param option_fn A function to process the option value.
#' @param envvar_fn A function to process the environment variable value.
#' @param quoted Whether the option value should be quoted.
#' @param eager Whether to evaluate the default value eagerly.
#' @param envir The environment in which to evaluate the option.
#' @param print_spec Whether to print the option specification.
#'
#' @return An invisible option specification object.
#'
#' @examples
#' option_spec_pkg("my_option", default = 42, desc = "An example option")
#'
#' @export
option_spec_pkg <- function(name, default = NULL, desc = NULL, option_name = NULL,
  envvar_name = NULL, option_fn = NULL, envvar_fn = NULL,
  quoted = FALSE, eager = FALSE, envir = parent.frame(),
  print_spec = TRUE) {
  if (is.null(option_name)) {
    option_name <- name
  }
  if (is.null(envvar_name)) {
    envvar_name <- toupper(paste0("R_", name))
  }

  if (eager) {
    default <- eval(default, envir = envir)
  }

  spec <- structure(list(
    name = name,
    expr = default,
    desc = desc,
    option_name = option_name,
    envvar_name = envvar_name,
    option_fn = option_fn,
    envvar_fn = envvar_fn,
    envir = envir
  ), class = "option_spec_pkg")

  if (print_spec) {
    output <- paste0(
      name, " =\n\n",
      "  ", desc, "\n\n",
      "  option  : ", option_name, "\n",
      "  envvar  : ", envvar_name, " (evaluated if possible, raw string otherwise)\n",
      " *default : ", deparse(default), "\n"
    )
    cat(output)
  }

  return(invisible(spec))
}

#' Define an option
#'
#' This function defines an option for a package. It attempts to set the option
#' in the package environment. If that fails (e.g., due to a locked namespace),
#' it falls back to setting a global option with a "zephyr." prefix.
#'
#' @inheritParams option_spec_pkg
#' @param option The name of the option to define.
#'
#' @return An invisible option specification object.
#'
#' @examples
#' define_option_pkg("my_option", default = 42, desc = "An example option")
#'
#' @export
define_option_pkg <- function(option, default = NULL, desc = NULL,
  option_name = NULL, envvar_name = NULL,
  option_fn = NULL, envvar_fn = NULL,
  quoted = FALSE, eager = FALSE,
  envir = parent.frame(), print_spec = TRUE) {
  spec <- option_spec_pkg(
    name = option,
    default = default,
    desc = desc,
    option_name = option_name,
    envvar_name = envvar_name,
    option_fn = option_fn,
    envvar_fn = envvar_fn,
    quoted = quoted,
    eager = eager,
    envir = envir,
    print_spec = print_spec
  )

  # Try to set the option in the package environment
  tryCatch({
    if (!exists(".options", envir = envir, inherits = FALSE)) {
      envir$.options <- new.env(parent = emptyenv())
    }
    envir$.options[[option]] <- spec
    packageStartupMessage(sprintf("Option '%s' successfully set in the package environment.", option))
  }, error = function(e) {
    # If we can't set it in the package environment, use a global option
    global_option_name <- paste0("zephyr.", option)
    options(stats::setNames(list(default), global_option_name))
    packageStartupMessage(sprintf("Option '%s' set as global option '%s'.", option, global_option_name))
  })

  invisible(spec)
}

#' Remove a package-specific option
#'
#' @description
#' This function removes a package-specific option that was previously set using
#' the `define_option_pkg` function.
#'
#' @param option A character string specifying the name of the option to remove.
#' @param envir The environment where the `.options` environment is located.
#'   Default is `parent.frame()`.
#'
#' @return Invisible NULL. The function is called for its side effect of removing the option.
#'
#' @details
#' The function removes the specified option from the `.options` environment
#' within the given environment. If the `.options` environment or the specified
#' option doesn't exist, the function will do nothing.
#'
#' @examples
#' \dontrun{
#' # Assuming we have previously set an option:
#' # define_option_pkg("my_option", default = "value")
#'
#' # To remove the option:
#' remove_option_pkg("my_option")
#'}
#' @export
remove_option_pkg <- function(option, envir = parent.frame()) {
  # Find the environment containing .options
  while (!is.null(envir) && !exists(".options", envir = envir, inherits = FALSE)) {
    envir <- parent.env(envir)
  }

  if (is.null(envir)) {
    message("No .options environment found in the calling stack.")
    return(invisible(NULL))
  }

  options_env <- get(".options", envir = envir)

  if (exists(option, envir = options_env, inherits = FALSE)) {
    rm(list = option, envir = options_env)
    message(sprintf("Option '%s' has been removed.", option))
  } else {
    message(sprintf("Option '%s' does not exist.", option))
  }

  invisible(NULL)
}

#' Determine the source of an option value
#'
#' This function determines the source of an option value (package environment, global option, environment variable, or default).
#'
#' @param spec An option specification object.
#' @param envir The environment in which to look for the option. Defaults to the parent frame.
#'
#' @return A character string indicating the source of the option value: "package", "option", "envvar", or "default".
#'
#' @examples
#' spec <- get_option_spec_pkg("verbosity_level", envir = getNamespace("zephyr"))
#' opt_source_pkg(spec)
#'
#' @export
opt_source_pkg <- function(spec, envir = parent.frame()) {
  if (is.null(spec) || !is.list(spec) || is.null(spec$name)) {
    stop("Invalid spec object")
  }

  pkg_name <- "zephyr"  # Assuming "zephyr" is the package name
  if (is.character(envir)) {
    pkg_name <- envir
  }

  # Check for package-specific global option first
  pkg_global_option <- paste0(pkg_name, ".", spec$name)
  if (!is.null(getOption(pkg_global_option))) {
    return("option")
  }

  # Check environment variable
  if (!is.null(spec$envvar_name) && nzchar(Sys.getenv(spec$envvar_name))) {
    return("envvar")
  }

  # Check R option
  if (!is.null(getOption(spec$name))) {
    return("option")
  }

  # Check if it's defined in the package environment
  if (is.environment(envir) && exists(spec$name, envir = envir, inherits = FALSE)) {
    return("package")
  }

  # If none of the above, it's using the default value
  return("default")
}

#' Get the value of an option
#'
#' This function retrieves the value of an option, considering its various possible sources.
#'
#' @param option_name The name of the option to retrieve.
#' @param default The default value to return if the option is not found.
#' @param envir The environment in which to look for the option. Can be:
#'   - An environment
#'   - A string specifying a package name
#'   - NULL (default), which uses the caller's environment
#'
#' @return The value of the option.
#'
#' @examples
#' define_option_pkg("my_option", default = 42)
#' opt_pkg("my_option")
#' opt_pkg("verbosity_level", envir = getNamespace("zephyr"))
#'
#' @export
opt_pkg <- function(option_name, default = NULL, envir = NULL) {
  # Determine the package name (if applicable)
  pkg_name <- "zephyr"  # Assuming "zephyr" is the package name

  # Determine the environment to use
  if (is.null(envir)) {
    envir <- parent.frame()
  } else if (is.character(envir)) {
    if (!requireNamespace(envir, quietly = TRUE)) {
      stop(paste("Package", envir, "is not available."))
    }
    envir <- asNamespace(envir)
    pkg_name <- envir  # Use the provided package name
  } else if (!is.environment(envir)) {
    stop("'envir' must be NULL, a string (package name), or an environment.")
  }

  # Try to get the spec from the package environment
  spec <- if (exists(".options", envir = envir, inherits = FALSE) &&
      exists(option_name, envir = envir$.options, inherits = FALSE)) {
    envir$.options[[option_name]]
  } else {
    NULL
  }

  # If spec is not found in the package environment, create a basic spec
  if (is.null(spec)) {
    spec <- list(
      name = option_name,
      option_name = option_name,
      envvar_name = toupper(paste0("R_", gsub(".", "_", option_name, fixed = TRUE)))
    )
  }

  # Check environment variable first
  if (!is.null(spec$envvar_name)) {
    env_value <- Sys.getenv(spec$envvar_name, unset = NA)
    if (!is.na(env_value)) {
      if (!is.null(spec$envvar_fn)) {
        return(spec$envvar_fn(env_value, spec$envvar_name))
      } else {
        return(env_value)
      }
    }
  }

  # Check for package-specific global option
  pkg_global_option <- paste0(pkg_name, ".", option_name)
  pkg_global_value <- getOption(pkg_global_option)
  if (!is.null(pkg_global_value)) {
    return(pkg_global_value)
  }

  # Check for regular option
  if (!is.null(spec$option_name)) {
    opt_value <- getOption(spec$option_name)
    if (!is.null(opt_value)) {
      return(opt_value)
    }
  }

  # Use default value
  if (is.null(spec$expr)) {
    return(default)
  } else {
    value <- if (isTRUE(spec$quoted)) spec$expr else eval(spec$expr, envir = spec$envir)
    if (!is.null(spec$option_fn)) {
      value <- spec$option_fn(value, x = option_name, default = default, env = envir, source = "default")
    }
    return(value)
  }
}

#' Get the option specification
#'
#' This function retrieves the option specification for a given option name.
#'
#' @param x The name of the option.
#' @param envir The environment in which to look for the option.
#' @param print_spec Whether to print the option specification.
#'
#' @return The option specification object, or NULL if not found.
#'
#' @examples
#' define_option_pkg("my_option", default = 42)
#' get_option_spec_pkg("my_option")
#'
#' @export
get_option_spec_pkg <- function(x, envir = parent.frame(), print_spec = FALSE) {
  if (!exists(".options", envir = envir, inherits = FALSE)) {
    return(NULL)
  }

  spec <- envir$.options[[x]]

  if (print_spec && !is.null(spec)) {
    cat(paste0(
      spec$name, " =\n\n",
      "  ", spec$desc, "\n\n",
      "  option  : ", spec$option_name, "\n",
      "  envvar  : ", spec$envvar_name, " (evaluated if possible, raw string otherwise)\n",
      " *default : ", deparse(spec$expr), "\n"
    ))
  }

  return(spec)
}

#' Get Package Options
#'
#' This function retrieves options set for a package or environment. It can return
#' just the names of the options, the option values, or full option details.
#'
#' @param envir The environment to search for options. Can be NULL (default, uses
#'   the calling environment), a string (package name), or an environment object.
#' @param names_only Logical. If TRUE, returns only the names of the options.
#'   Default is FALSE.
#' @param full Logical. If TRUE, returns the full option specifications including
#'   all details. Default is FALSE.
#'
#' @return Depending on the parameters:
#'   - If `names_only = TRUE`: A character vector of option names.
#'   - If `full = TRUE`: A list of full option specifications.
#'   - Otherwise: A list of option values (default behavior).
#'
#' @examples
#' \dontrun{
#' # Assuming 'mypackage' has some options set
#' # Get option names
#' opts_pkg("mypackage", names_only = TRUE)
#'
#' # Get option values (default behavior)
#' opts_pkg("mypackage")
#'
#' # Get full option details
#' opts_pkg("mypackage", full = TRUE)
#' }
#'
#' @export
opts_pkg <- function(envir = NULL, names_only = FALSE, full = FALSE) {
  # Determine the environment to use
  if (is.null(envir)) {
    envir <- parent.frame()
  } else if (is.character(envir)) {
    if (!requireNamespace(envir, quietly = TRUE)) {
      stop(paste("Package", envir, "is not available."))
    }
    envir <- asNamespace(envir)
  } else if (!is.environment(envir)) {
    stop("'envir' must be NULL, a string (package name), or an environment.")
  }

  # Check if the .options environment exists in the specified environment
  if (!exists(".options", envir = envir, inherits = FALSE)) {
    # If not, look for global options starting with "zephyr."
    all_options <- options()
    zephyr_options <- all_options[grep("^zephyr\\.", names(all_options))]

    if (length(zephyr_options) == 0) {
      return(if (names_only) character(0) else list())  # Return empty vector or list
    }

    if (names_only) {
      return(sub("^zephyr\\.", "", names(zephyr_options)))
    }

    if (full) {
      return(lapply(zephyr_options, function(opt) {
        list(name = sub("^zephyr\\.", "", names(opt)), expr = opt, envir = .GlobalEnv)
      }))
    }

    return(zephyr_options)
  }

  # Get the .options environment
  opt_env <- get(".options", envir = envir, inherits = FALSE)

  # List all objects in the .options environment
  option_names <- ls(opt_env, all.names = TRUE)

  if (names_only) {
    return(option_names)
  }

  # Create a list of all options and their values
  options_list <- lapply(option_names, function(name) {
    opt <- get(name, envir = opt_env)
    if (full) {
      opt
    } else {
      if (is.null(opt$expr)) {
        NULL
      } else if (isTRUE(opt$quoted)) {
        opt$expr
      } else {
        tryCatch(eval(opt$expr, envir = opt$envir), error = function(e) NULL)
      }
    }
  })
  names(options_list) <- option_names

  return(options_list)
}

#' Generate Roxygen Documentation for Package Options
#'
#' This function creates Roxygen2 documentation for all options of a specified package.
#' It formats the options' descriptions, default values, and environment variables
#' in a structured manner suitable for inclusion in package documentation.
#'
#' @param pkg A character string specifying the name of the package.
#'
#' @return A character vector containing Roxygen2 documentation for the package options.
#'
#' @details
#' The function performs the following steps:
#' 1. Retrieves all options for the specified package.
#' 2. Formats each option's description, default value, and associated environment variable.
#' 3. Wraps long text to improve readability in the documentation.
#' 4. Structures the documentation using Roxygen2 and LaTeX formatting commands.
#'
#' The resulting documentation includes:
#' - A description of each option
#' - The default value (formatted as code)
#' - The option name for use in `options()`
#' - The associated environment variable name and its behavior
#'
#' @note This function is intended for internal use in package development
#' and documentation generation processes.
#'
#' @examples
#' \dontrun{
#' # Generate documentation for a package named "mypackage"
#' docs <- as_roxygen_docs_pkg("mypackage")
#'
#' # Print the generated documentation
#' cat(paste(docs, collapse = "\n"))
#' }
#'
#' @seealso [base::options()], [base::Sys.getenv()]
#'
#' @export
as_roxygen_docs_pkg <- function(pkg) {
  # Get all options with full details
  if (is.environment(pkg)) {
    options <- opts_pkg(pkg, full = TRUE)
    pkg_name <- environmentName(pkg)
    if (pkg_name == "") pkg_name <- "package"
  } else {
    options <- opts_pkg(pkg, full = TRUE)
    pkg_name <- pkg
  }

  # Get all options with full details
  options <- opts_pkg(pkg, full = TRUE)

  # Function to wrap text
  wrap_text <- function(text, width = 80) {
    wrapped <- strwrap(text, width = width, exdent = 2)
    paste(wrapped, collapse = "\n")
  }

  # Function to format default values as code with wrapping
  format_default <- function(expr) {
    if (is.null(expr)) return("NULL")

    # Use deparse with a large width to avoid unintended line breaks
    default_str <- paste(deparse(expr, width.cutoff = 500), collapse = " ")

    # Wrap the text
    wrapped_str <- wrap_text(default_str, width = 76)  # 76 to account for indentation

    # Wrap in \preformatted{} for code formatting
    paste0("\\preformatted{\n", wrapped_str, "\n}")
  }

  # Create the options section
  options_section <- c(
    "@section Options:",
    "\\describe{"
  )

  # Add each option to the options section
  for (param in names(options)) {
    opt <- options[[param]]
    options_section <- c(options_section,
      paste0("  \\item{\\code{", param, "}}{"),
      paste0("    ", wrap_text(opt$desc)),
      "    \\itemize{",
      paste0("      \\item Default: ", format_default(opt$expr)),
      paste0("      \\item Option: \\code{", pkg_name, ".", param, "}"),
      paste0("      \\item Environment variable: \\code{", opt$envvar_name, "} (",
        wrap_text(if(is.logical(opt$expr)) "TRUE if one of 'TRUE', '1', FALSE otherwise"
          else if(is.character(opt$expr)) "evaluated if possible, raw string otherwise"
          else if(is.numeric(opt$expr)) "evaluated if possible, raw string otherwise"
          else "as character vector, split on ';' delimiter", width = 60), ")"),
      "    }",
      "  }"
    )
  }

  options_section <- c(options_section, "}")

  # Return as a character vector
  return(options_section)
}

#' Generate a character vector of parameters for package options
#'
#' This function generates a character vector of parameters for options defined using
#' the custom options functions. The resulting vector can be used in documentation
#' or for other purposes.
#'
#' @param envir The environment containing the options. Can be NULL (default,
#'   uses the calling environment), a string (package name), or an environment object.
#' @param include A character vector of option names to include. If NULL (default),
#'   all options are included.
#' @param exclude A character vector of option names to exclude. Default is NULL.
#'
#' @return A character vector of parameter descriptions, where each element is a string
#'   describing an option.
#'
#' @examples
#' \dontrun{
#' # Assuming some options are defined in the current environment
#' params <- as_params_pkg()
#' cat(params, sep = "\n")
#'
#' # For a specific package
#' params <- as_params_pkg("mypackage")
#' cat(params, sep = "\n")
#'
#' # Include only specific options
#' params <- as_params_pkg(include = c("option1", "option2"))
#' cat(params, sep = "\n")
#'
#' # Exclude specific options
#' params <- as_params_pkg(exclude = c("option3", "option4"))
#' cat(params, sep = "\n")
#' }
#'
#' @export
as_params_pkg <- function(envir = NULL, include = NULL, exclude = NULL) {
  # Determine the environment to use
  if (is.null(envir)) {
    envir <- parent.frame()
  } else if (is.character(envir)) {
    if (!requireNamespace(envir, quietly = TRUE)) {
      stop(paste("Package", envir, "is not available."))
    }
    envir <- asNamespace(envir)
  } else if (!is.environment(envir)) {
    stop("'envir' must be NULL, a string (package name), or an environment.")
  }

  # Get all options with full details
  options <- opts_pkg(envir, full = TRUE)

  # Filter options based on include and exclude parameters
  if (!is.null(include)) {
    options <- options[names(options) %in% include]
  }
  if (!is.null(exclude)) {
    options <- options[!names(options) %in% exclude]
  }

  # Generate the parameter descriptions
  params <- vapply(names(options), function(name) {
    opt <- options[[name]]

    # Create the parameter description
    desc <- c(
      paste0("@param ", name, " ", opt$desc),
      paste0("  Default: ", deparse(opt$expr)),
      paste0("  Option: ", opt$option_name),
      paste0("  Environment variable: ", opt$envvar_name)
    )

    # Combine the description into a single string
    paste(desc, collapse = "\n")
  }, FUN.VALUE = character(1))

  # Return the character vector
  return(params)
}

#' Check if an environment variable or option is set to a truthy value
#'
#' This function returns another function that checks if a given environment variable
#' or option is set to a value that can be interpreted as true. It first checks for
#' an environment variable of the form R_OPTION_NAME or the exact name provided,
#' then falls back to the value set in the specified environment.
#'
#' @param envir The environment in which to look for the option. Can be NULL (default, uses .GlobalEnv),
#'   a string (package name), or an environment.
#'
#' @return A function that takes a parameter 'x' (the name of the option or environment variable to check)
#'   and returns a logical value: TRUE if the option or environment variable is set to a truthy value,
#'   FALSE otherwise.
#'
#' @examples
#' is_true <- envvar_is_true_pkg()
#' Sys.setenv(R_MY_OPTION = "true")
#' is_true("my_option")  # Returns TRUE
#'
#' Sys.setenv(R_MY_OPTION = "false")
#' is_true("my_option")  # Returns FALSE
#'
#' Sys.setenv(MY_ENV_VAR = "yes")
#' is_true("MY_ENV_VAR")  # Returns TRUE
#'
#' @export
envvar_is_true_pkg <- function(envir = NULL) {
  if (is.null(envir)) {
    envir <- .GlobalEnv
  } else if (is.character(envir)) {
    if (!requireNamespace(envir, quietly = TRUE)) {
      stop(paste("Package", envir, "is not available."))
    }
    envir <- asNamespace(envir)
  } else if (!is.environment(envir)) {
    stop("'envir' must be NULL, a string (package name), or an environment.")
  }

  fn <- function(x) {
    # First, check for the environment variable with R_ prefix
    envvar_name <- paste0("R_", toupper(x))
    envvar_value <- Sys.getenv(envvar_name, unset = NA)

    # If not found, check for the exact name provided
    if (is.na(envvar_value)) {
      envvar_value <- Sys.getenv(x, unset = NA)
    }

    if (!is.na(envvar_value)) {
      value <- envvar_value
    } else {
      # If environment variable is not set, check for the option
      if (exists(x, envir = envir, inherits = FALSE)) {
        value <- get(x, envir = envir)
      } else {
        # If neither environment variable nor option is set, return FALSE
        return(FALSE)
      }
    }

    if (is.logical(value)) {
      return(value)
    } else if (is.character(value) && !is.na(value)) {
      return(tolower(value) %in% c("true", "t", "yes", "y", "1"))
    } else {
      return(FALSE)
    }
  }

  attr(fn, "desc") <- "TRUE if one of 'TRUE', 'T', 'YES', 'Y', '1' (case-insensitive), FALSE otherwise"
  return(fn)
}

#' Split an environment variable or option string into a vector
#'
#' This function returns another function that splits a given string (from an environment variable or option)
#' into a vector using a specified delimiter, and trims whitespace.
#'
#' @param delim The delimiter to use for splitting the string. Default is ",".
#' @param envir The environment in which to look for the option. Can be NULL (default, uses .GlobalEnv),
#'   a string (package name), or an environment.
#'
#' @return A function that takes parameters:
#'   - x: The name of the option or environment variable to check
#'   - ...: Additional arguments (currently unused)
#'
#' @examples
#' split_fn <- envvar_str_split_pkg()
#' Sys.setenv(R_MY_LIST_OPTION = "item1,item2,item3")
#' split_fn("my_list_option")  # Returns c("item1", "item2", "item3")
#'
#' Sys.setenv(MY_ENV_LIST = "x|y|z")
#' split_fn <- envvar_str_split_pkg(delim = "|")
#' split_fn("MY_ENV_LIST")  # Returns c("x", "y", "z")
#'
#' @export
envvar_str_split_pkg <- function(delim = ",", envir = NULL) {
  if (is.null(envir)) {
    envir <- .GlobalEnv
  } else if (is.character(envir)) {
    if (!requireNamespace(envir, quietly = TRUE)) {
      stop(paste("Package", envir, "is not available."))
    }
    envir <- asNamespace(envir)
  } else if (!is.environment(envir)) {
    stop("'envir' must be NULL, a string (package name), or an environment.")
  }

  fn <- function(x, ...) {
    # Check environment variable first
    envvar_name <- paste0("R_", toupper(x))
    envvar_value <- Sys.getenv(envvar_name, unset = NA)

    if (!is.na(envvar_value)) {
      value <- envvar_value
    } else if (exists(x, envir = envir, inherits = FALSE)) {
      # If environment variable is not set, check for the option in the specified environment
      value <- get(x, envir = envir)
    } else {
      # If neither environment variable nor option is set, return NULL
      return(NULL)
    }

    if (is.null(value) || is.na(value) || value == "") {
      return(NULL)
    }

    # Convert to character if it's not already
    value <- as.character(value)

    # If the value doesn't contain the delimiter, return it as is
    if (!grepl(delim, value, fixed = TRUE)) {
      return(value)
    }

    result <- trimws(strsplit(value, delim, fixed = TRUE)[[1]])
    result[nzchar(result)]
  }

  attr(fn, "desc") <- sprintf("as character vector, split on '%s' delimiter", delim)
  return(fn)
}
