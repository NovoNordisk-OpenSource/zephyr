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
  quoted = FALSE, eager = FALSE, envir = parent.frame()) {
  if (is.null(option_name)) {
    option_name <- option
  }
  if (is.null(envvar_name)) {
    envvar_name <- toupper(paste0("R_", option))
  }

  if (eager) {
    default <- eval(default, envir = envir)
  }

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
    print_spec = FALSE
  )

  output <- paste0(
    "\n",
    option, " = \"", default, "\"\n\n",
    "  ", desc, "\n\n",
    "  option  : ", option_name, "\n",
    "  envvar  : ", envvar_name, " (evaluated if possible, raw string otherwise)\n",
    " *default : \"", default, "\"\n"
  )
  cat(output)

  if (!exists(".options", envir = envir, inherits = FALSE)) {
    tryCatch({
      envir$.options <- new.env(parent = emptyenv())
    }, error = function(e) {
      .options_temp <- new.env(parent = emptyenv())
      assign(".options", .options_temp, envir = envir)
    })
  }

  tryCatch({
    envir$.options[[option]] <- spec
  }, error = function(e) {
    options_env <- get(".options", envir = envir)
    assign(option, spec, envir = options_env)
  })

  return(invisible(spec))
}

#' Determine the source of an option value
#'
#' This function determines the source of an option value (option, environment variable, or default).
#'
#' @param x The name of the option or an option specification object.
#' @param envir The environment in which to look for the option.
#'
#' @return A character string indicating the source of the option value.
#'
#' @examples
#' define_option_pkg("my_option", default = 42)
#' opt_source_pkg("my_option")
#'
#' @export
opt_source_pkg <- function(x, envir = parent.frame()) {
  if (!inherits(x, "option_spec_pkg")) {
    x <- get_option_spec_pkg(x, envir = envir, print_spec = FALSE)
  }

  if (is.null(x)) {
    return(NA_character_)
  }

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

#' Get the value of an option
#'
#' This function retrieves the value of an option, considering its various possible sources.
#'
#' @param option_name The name of the option to retrieve.
#' @param default The default value to return if the option is not found.
#' @param envir The environment in which to look for the option.
#'
#' @return The value of the option.
#'
#' @examples
#' define_option_pkg("my_option", default = 42)
#' opt_pkg("my_option")
#'
#' @export
opt_pkg <- function(option_name, default = NULL, envir = parent.frame()) {
  spec <- get_option_spec_pkg(option_name, envir = envir, print_spec = FALSE)

  if (is.null(spec)) {
    return(default)
  }

  source <- opt_source_pkg(spec, envir = envir)

  value <- switch(source,
    envvar = spec$envvar_fn(Sys.getenv(spec$envvar_name, unset = NA), spec$envvar_name),
    option = getOption(spec$option_name),
    default = eval(spec$expr, envir = spec$envir),
    stop(sprintf("Option '%s' not found.", option_name))
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
get_option_spec_pkg <- function(x, envir = parent.frame(), print_spec = TRUE) {
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

