#' Get option using the `options` package, allowing a global option
#'
#' @description
#' Get option using the `options` package, where the
#' function allows a global option to overwrite individual package options.
#'
#' @param opt_name `character` name of the option
#' @param env `environment` the environment to get the option from. As default
#' the namespace of the package where the function at the top of the call stack
#' comes from
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
get_opt <- function(opt_name = NULL,
                    env = parent.frame()) {

  # If a global option is set, use that
  global_opt <- getOption(paste0("zephyr.", opt_name))
  if (!is.null(global_opt)) {
    return(global_opt)
  }

  # Get the option from the namespace of the package used "from start"
  return(options::opt(opt_name, env = env))
}
