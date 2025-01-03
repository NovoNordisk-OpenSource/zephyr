#' Verbosity level
#' @name verbosity_level
NULL

create_option(
  name = "verbosity_level",
  default = "verbose",
  description = "test"
  )

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
#' @examples
#' # Get the verbosity level
#' # Note: Specifying the environment is not needed when used inside a package
#' get_verbosity_level("zephyr")
#'
#' # Using withr::with_envvar to temporarily set verbosity level
#' withr::with_envvar(
#'   new = c("R_ZEPHYR_VERBOSITY_LEVEL" = "quiet"),
#'   code = get_verbosity_level("zephyr")
#'   )
#'
#' @export
get_verbosity_level <-  function(.envir = sys.function(which = -1)) {
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
