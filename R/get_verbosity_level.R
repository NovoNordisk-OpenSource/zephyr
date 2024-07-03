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
#' # Setting a "global option" overwrites the behavior, i.e. example below will
#' # never write anything to the console no matter the option set in the
#' # package where the filter_data function is located (as an example this package
#' # is here called "newpackage")
#'
#' filter_data <- function(data, infilter, ...) {
#'
#'   #Defusing the filter arguments
#'   infilter_e <- rlang::enquo(infilter)
#'   infilter_lb <- rlang::as_label(infilter_e)
#'
#'   msg("Filtering {.field data} by {.field {infilter_lb}}",
#'     levels_to_write = c("verbose", "debug"),
#'     msg_fun = cli::cli_h2)
#'
#'   #Adding a debug message that only will appear if verbosity level is set to
#'   "debug"
#'   msg_debug("Trying to filter data")
#'
#'   data |>
#'     dplyr::filter({{infilter}})
#'
#'   msg_success("Data filtered by {.field {infilter_lb}}")
#'  }
#'
#' withr::with_options(
#' list(zephyr.verbosity_level = "verbose",
#'      newpacakge.verbosity_level = "verbose"),
#'      filter_data(data = cars, infilter = speed > 12)
#' )
#'
#' withr::with_options(
#'   list(zephyr.verbosity_level = "quiet",
#'        newpacakge.verbosity_level = "verbose"),
#'        filter_data(data = cars, infilter = speed > 12)
#' )
#'
get_verbosity_level <- function(env = parent.frame()) {

  zephyr_verbosity_level_source <- options::opt_source("verbosity_level",
                                                       env = getNamespace("zephyr"))
  zephyr_verbosity_level <- options::opt("verbosity_level", env = getNamespace("zephyr"))

  # If option is not defined for the specified environment, we use the option
  # set on zephyr level (using default is nothing is done)
  if (is.null(attr(env$.options, "spec"))) {
    return(zephyr_verbosity_level)
  }

  pkg_verbosity_level_source <- options::opt_source("verbosity_level",
                                                    env = env)
  pkg_verbosity_level <- options::opt("verbosity_level", env = env)

  # If option is set on "zephyr level" use that one
  if (zephyr_verbosity_level_source == "option") {
    return(zephyr_verbosity_level)
  }

  # If option is set on "env level" use that one
  if (pkg_verbosity_level_source == "option") {
    return(pkg_verbosity_level)
  }

  # If option is not set as an option on package level, and zephyr option is set
  # using environment variable, use that one
  if (zephyr_verbosity_level_source == get_envname_options()) {
    return(zephyr_verbosity_level)
  }

  # If zephyr level option is not set, use the option set on package level, whether
  # default or envvar
  return(pkg_verbosity_level)
}

#' Get the value of the `opt_source` for an environment variable based on `options` package version
#'
#' See breaking change in version 0.2.0 in the changelog here:
#' https://dgkf.github.io/options/news/index.html. Function gives the value that is
#' returned by [opt_source()]. Is used inside the [get_verbosity_level()] function.
#'
#' @return `character` with the value of the `opt_source` for an environment variable
#'
#' @export
#'
#' @examples
#' get_envname_options()
get_envname_options <- function() {
  options_version_numeric <- gsub("\\.", "", as.character(utils::packageVersion("options"))) |>
    as.numeric()

  if (options_version_numeric < 20) {
    "envir"
  } else {
    "envvar"
  }
}
