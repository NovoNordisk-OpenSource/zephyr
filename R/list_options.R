#' List package options
#'
#' @description
#' List all `zephyr_options` specified in a package. Either as an `list` or as
#' as `character` vector formatted for use in your package documentation.
#'
#' To document your options use `use_zephyr()` to set everything up, and edit
#' the created template as necessary.
#'
#' @param as `[character(1)]` Format in which to return the options:
#' * `"list"`: Return a nested list, where each top level element is a list with
#' the specification of an option.
#' * `"params"`: Return a character vector with the `"@param"` tag entries for each
#' option similar to how function parameters are documented with roxygen2.
#' * `"markdown"`: Return a character string with markdown formatted entries for
#' each option.
#' @param .envir Environment in which the options are defined.
#' Default is suitable for use inside your package.
#' @examples
#' # List all options in zephyr
#' x <- list_options(.envir = "zephyr")
#' print(x)
#' str(x)
#'
#' # Create @params tag entries for each option
#' list_options(as = "params", .envir = "zephyr") |>
#'   cat(sep = "\n")
#'
#' # List options in markdown format
#' list_options(as = "markdown", .envir = "zephyr") |>
#'   cat()
#' @export
list_options <- function(as = c("list", "params", "markdown"),
                         .envir = sys.function(which = -1)) {
  as <- rlang::arg_match(as)

  env <- envname(.envir)
  options <- getNamespace(env)[[".zephyr_options"]]

  if (as == "params") {
    options <- options |>
      vapply(
        FUN = glue::glue_data,
        FUN.VALUE = character(1),
        "@param {name} {description}. Default: `{default}`. See [{environment}-options] for more information.",
        USE.NAMES = FALSE
      )
  } else if (as == "markdown") {
    options <- options |>
      vapply(
        FUN = glue::glue_data,
        FUN.VALUE = character(1),
        "
        ## {name}
        {description}
        * Default: `{default}`
        * Option: `{tolower(environment)}.{name}`
        * Environment: `R_{toupper(environment)}_{toupper(name)}`
        ",
        USE.NAMES = FALSE
      ) |>
      paste(collapse = "\n")
  }

  return(options)
}
