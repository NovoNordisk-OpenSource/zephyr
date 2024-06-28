#' Get `verbosity_level` option using the `options` package, allowing a global option
#'
#' @description
#' Get option using the `options` package, where the
#' function allows a global option to overwrite individual package options.
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
get_verbosity_level <- function(env = parent.frame()) {

  zephyr_verbosity_level_source <- options::opt_source("verbosity_level",
                                                       env = getNamespace("zephyr"))
  zephyr_verbosity_level <- options::opt("verbosity_level", env = getNamespace("zephyr"))

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
  if (zephyr_verbosity_level_source == "envvar") {
    return(zephyr_verbosity_level)
  }

  # If zephyr level option is not set, use the option set on package level, whether
  # default or envvar
  return(pkg_verbosity_level)
}
