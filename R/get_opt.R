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
#' # callisto package
#' \dontrun{
#' withr::with_options(list(zephyr.verbosity_level = "quiet",
#'                          callisto.verbosity_level = "verbose"),
#'                     callisto::filter_with_popdata(
#'                        pharmaverseadam::adlb,
#'                        infilter = PARAMCD == "BILIS",
#'                        popdata = pharmaverseadam::adsl,
#'                        popfilter = SAFFL == "Y"))
#' }
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
