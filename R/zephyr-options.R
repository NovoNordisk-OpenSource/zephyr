#' Get the value of an option.
#'
#' This function retrieves the value of an option.
#'
#' @param option_name The name of the option.
#' @param default The default value to return if the option is not found.
#' @param envir The environment to search for the option.
#'
#' @return The value of the option.
#'
#' @export
opt_pkg <- function(option_name, default = NULL, envir = parent.frame()) {
  # This function retrieves the value of an option.

  # 1. Get the option specification.
  spec <- get_option_spec_pkg(option_name, envir = envir, print_spec = FALSE)

  # 2. Determine the source of the option value.
  source <- opt_source_pkg(spec, envir = envir)

  # 3. Retrieve the option value based on its source.
  value <- switch(source,
    envvar = spec$envvar_fn(Sys.getenv(spec$envvar_name, unset = NA), spec$envvar_name),
    option = getOption(spec$option_name),
    default = eval(spec$expr, envir = spec$envir),
    stop(sprintf("Option '%s' not found.", option_name))
  )

  # 4. Apply the option function (if any) to the value.
  if (!is.null(spec$option_fn)) {
    value <- spec$option_fn(value, x = option_name, default = default, env = envir, source = source)
  }

  return(value)
}

#' Determine the source of an option value.
#'
#' This function determines the source of an option value.
#'
#' @param x An option specification or the name of an option.
#' @param envir The environment to search for the option.
#'
#' @return The source of the option value ("option", "envvar", "default", or NA).
#'
#' @export
opt_source_pkg <- function(x, envir = parent.frame()) {
  # This function determines the source of an option value.

  # 1. If x is not an option specification, get the specification (suppress printing).
  if (!inherits(x, "option_spec_pkg")) {
    x <- get_option_spec_pkg(x, envir = envir, print_spec = FALSE)
  }

  # 2. Check if the option is set in any of the possible sources.
  if (x$option_name %in% names(options())) {
    return("option")
  } else if (!is.na(Sys.getenv(x$envvar_name, unset = NA))) {
    return("envvar")
  } else if (!is.null(x$expr)) {
    return("default")
  } else {
    return(NA_character_)
  }
}

#' Create an option specification.
#'
#' This function creates an option specification.
#'
#' @param name The name of the option.
#' @param default The default value of the option.
#' @param desc A description of the option.
#' @param option_name The name of the option in the options list.
#' @param envvar_name The name of the environment variable.
#' @param option_fn A function to apply to the option value.
#' @param envvar_fn A function to apply to the environment variable value.
#' @param quoted Whether the default value is quoted.
#' @param eager Whether to evaluate the default value eagerly.
#' @param envir The environment to evaluate the default value in.
#' @param print_spec Whether to print the option specification.
#'
#' @return An option specification object (invisibly).
#'
#' @export
option_spec_pkg <- function(name, default = NULL, desc = NULL, option_name = NULL,
  envvar_name = NULL, option_fn = NULL, envvar_fn = NULL,
  quoted = FALSE, eager = FALSE, envir = parent.frame(),
  print_spec = TRUE) { # Add print_spec argument
  # This function creates an option specification.

  # 1. Set default values for option_name and envvar_name.
  if (is.null(option_name)) {
    option_name <- name
  }
  if (is.null(envvar_name)) {
    envvar_name <- toupper(paste0("R_", name))
  }

  # 2. Evaluate the default value if eager evaluation is requested.
  if (eager) {
    default <- eval(default, envir = envir)
  }

  # 3. Create the option specification.
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

  # 4. Create and format the option specification output
  if (print_spec) { # Conditionally print the spec
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

#' Define an option.
#'
#' This function defines an option.
#'
#' @param option The name of the option.
#' @param default The default value of the option.
#' @param desc A description of the option.
#' @param option_name The name of the option in the options list.
#' @param envvar_name The name of the environment variable.
#' @param option_fn A function to apply to the option value.
#' @param envvar_fn A function to apply to the environment variable value.
#' @param quoted Whether the default value is quoted.
#' @param eager Whether to evaluate the default value eagerly.
#' @param envir The environment to evaluate the default value in.
#'
#' @return An option specification object (invisibly).
#'
#' @export
define_option_pkg <- function(option, default = NULL, desc = NULL, option_name = NULL,
  envvar_name = NULL, option_fn = NULL, envvar_fn = NULL,
  quoted = FALSE, eager = FALSE, envir = parent.frame()) {
  # This function defines an option.

  # 1. Set default values for option_name and envvar_name.
  if (is.null(option_name)) {
    option_name <- option
  }
  if (is.null(envvar_name)) {
    envvar_name <- toupper(paste0("R_", option))
  }

  # 2. Evaluate the default value if eager evaluation is requested.
  if (eager) {
    default <- eval(default, envir = envir)
  }

  # 3. Create the option specification (set print_spec = FALSE)
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
    print_spec = FALSE # Do not print the spec here
  )

  # 4. Format the option specification output
  output <- paste0(
    "\n",
    option, " = \"", default, "\"\n\n",
    "  ", desc, "\n\n",
    "  option  : ", option_name, "\n",
    "  envvar  : ", envvar_name, " (evaluated if possible, raw string otherwise)\n",
    " *default : \"", default, "\"\n"
  )
  cat(output)

  return(invisible(spec))
}

#' Get an option specification.
#'
#' This function retrieves an option specification.
#'
#' @param option_name The name of the option.
#' @param envir The environment to search for the option.
#' @param print_spec Whether to print the option specification.
#'
#' @return An option specification object.
#'
#' @export
get_option_spec_pkg <- function(option_name, envir = parent.frame(), print_spec = TRUE) {
  # This function retrieves the option specification for the given name.

  # You'll need to adapt this to your actual implementation.
  # Here's a simple example using a list to store option specifications.

  option_specs <- list(
    verbosity_level = option_spec_pkg(
      name = "verbosity_level",
      default = "verbose",
      desc = "Controls verbosity level",
      option_name = "verbosity_level",
      envvar_name = "R_VERBOSITY_LEVEL",
      print_spec = print_spec # Pass print_spec argument
    )
    # Add more option specifications as needed
  )

  return(option_specs[[option_name]])
}
