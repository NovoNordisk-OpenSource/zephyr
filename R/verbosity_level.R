#' Verbosity level to control package behavior
#' @description
#' In zephyr we define a central verbosity level to control the amount of
#' messages the user receives when using zephyr and other packages
#' in the ecosystem.
#'
#' Verbosity level can be any of the four values below:
#'
#' 1. `quiet`: No messages are displayed.
#' 1. `minimal`: Only essential messages are displayed.
#' 1. `verbose` (*default*): More informative messages are displayed.
#' 1. `debug`: Detailed messages for debugging are displayed.
#'
#' See [use_zephyr()] and [msg] for how to implement the use of verbosity levels
#' in your package and its functions.
#'
#' Verbosity level is a special kind of option that can be scoped both for a
#' specific package and and globally for the ecosystem
#' (assigned to the zephyr package).
#' It can be set using either R `options()` or environment variables.
#'
#' Verbosity level is retrieved using the [get_verbosity_level()] function.
#' Since the level can have multiples scopes, the following hierarchy is used:
#'
#' 1. Package specific option: `{pkgname}.verbosity_level`
#' 1. Package specific environment variable: `R_{PKGNAME}_VERBOSITY_LEVEL`
#' 1. Ecosystem wide option: `zephyr.verbosity_level`
#' 1. Ecosystem wide environment variable (`R_ZEPHYR_VERBOSITY_LEVEL`)
#' 1. Default value specified in zephyr (`verbose`, see above).
#'
#' In order to see all registered verbosity levels across scopes call
#' [get_all_verbosity_levels()].
#' @examples
#' get_verbosity_level("zephyr")
#' get_all_verbosity_levels()
#' @name verbosity_level
NULL

create_option(
  name = "verbosity_level",
  default = "verbose",
  description = "test"
)

#' Get all verbosity levels
#' @description
#' Retrieves all active verbosity levels set for loaded packages.
#'
#' See also [verbosity_level] and [get_verbosity_level()].
#'
#' @returns Named `[character()]` vector with package as names and their
#' verbosity levels as values.
#' @examples
#' get_all_verbosity_levels()
#' @export
get_all_verbosity_levels <- function() {
  envs <- loadedNamespaces()
  names(envs) <- envs

  lapply(
    X = envs,
    FUN = \(x) get_option(name = "verbosity_level", .envir = x)
  ) |>
    unlist()
}

#' Get verbosity level
#' @description
#' This function retrieves the `verbosity_level` for your environment using the
#' priority hierarchy as described in [verbosity_level].
#'
#' While the examples use `zephyr`, this function works with any package,
#' and inside a package it is not necessary to specify it; the default value
#' of `.envir` is enough.
#'
#' It is normally not relevant to query the `verbosity_level` yourself. Instead
#' use the appropriate [msg] function.
#'
#' @param .envir Environment in which the options are defined.
#' Default is suitable for use inside your package.
#' @returns `[character(1)]` representing the verbosity level.
#' @examples
#' # Get the verbosity level
#' # Note: Specifying the environment is not needed when used inside a package
#' get_verbosity_level("zephyr")
#'
#' # Temporarily change verbosity level using an environment variable
#' withr::with_envvar(
#'   new = c("R_ZEPHYR_VERBOSITY_LEVEL" = "quiet"),
#'   code = get_verbosity_level("zephyr")
#' )
#'
#' # Temporarily change verbosity level using an option value
#' withr::with_options(
#'   new = c("zephyr.verbosity_level" = "minimal"),
#'   code = get_verbosity_level("zephyr")
#' )
#'
#' @export
get_verbosity_level <- function(.envir = sys.function(which = -1)) {
  coalesce_dots(
    get_option(name = "verbosity_level", .envir = .envir),
    get_option(name = "verbosity_level", .envir = "zephyr")
  ) |>
    validate_verbosity_level()
}

#' @noRd
validate_verbosity_level <- function(level) {
  valid_levels <- c("quiet", "minimal", "verbose", "debug")
  level <- tolower(level)
  if (level %in% valid_levels) {
    return(level)
  } else {
    cli::cli_alert_warning(
      "Invalid verbosity level {.val {level}}. Using {.val verbose}"
    )
    return("verbose")
  }
}
