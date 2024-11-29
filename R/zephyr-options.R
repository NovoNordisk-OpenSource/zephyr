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
#' This function defines an option and stores its specification in the environment.
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
define_option_pkg <- function(option, default = NULL, desc = NULL, option_name = NULL,
  envvar_name = NULL, option_fn = NULL, envvar_fn = NULL,
  quoted = FALSE, eager = FALSE, envir = parent.frame(), print_spec = TRUE) {
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

  if (!exists(".options", envir = envir, inherits = FALSE)) {
    envir$.options <- new.env(parent = emptyenv())
  }

  envir$.options[[option]] <- spec

  return(invisible(spec))
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
#' # Assuming we have previously set an option:
#' # define_option_pkg("my_option", default = "value")
#'
#' # To remove the option:
#' remove_option_pkg("my_option")
#'
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
#' This function determines the source of an option value (option, environment variable, or default).
#'
#' @param spec An option specification object.
#' @param envir The environment in which to look for the option.
#'
#' @return A character string indicating the source of the option value.
#'
#' @examples
#' define_option_pkg("my_option", default = 42)
#' spec <- get_option_spec_pkg("my_option")
#' opt_source_pkg(spec)
#'
#' @export
opt_source_pkg <- function(spec, envir = parent.frame()) {
  if (!is.null(spec$option_name) && !is.null(getOption(spec$option_name))) {
    return("option")
  }

  if (!is.null(spec$envvar_name) && nzchar(Sys.getenv(spec$envvar_name))) {
    return("envvar")
  }

  if (!is.null(spec$expr)) {
    return("default")
  }

  return(NULL)
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
#' opt_pkg("verbosity_level", envir = "zephyr")
#'
#' @export
opt_pkg <- function(option_name, default = NULL, envir = NULL) {
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

  spec <- get_option_spec_pkg(option_name, envir = envir, print_spec = FALSE)

  if (is.null(spec)) {
    return(default)
  }

  source <- opt_source_pkg(spec, envir = envir)

  value <- switch(source,
    envvar = {
      env_value <- Sys.getenv(spec$envvar_name, unset = NA)
      if (!is.null(spec$envvar_fn)) spec$envvar_fn(env_value, spec$envvar_name) else env_value
    },
    option = {
      getOption(spec$option_name)
    },
    default = {
      eval(spec$expr, envir = spec$envir)
    },
    default
  )

  if (!is.null(spec$option_fn)) {
    value <- spec$option_fn(value, x = option_name, default = default, env = envir, source = source)
  }

  return(value)
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
    return(if (names_only) character(0) else list())  # Return empty vector or list
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
      opt$expr
    }
  })
  names(options_list) <- option_names

  return(options_list)
}

