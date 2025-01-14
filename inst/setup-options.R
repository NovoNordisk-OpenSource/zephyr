#' @title Options for {pkg}
#' @name {pkg}-options
#' @description
#' `r zephyr::list_options(as = "markdown", .envir = "{pkg}")`
NULL

#' @title Internal parameters for reuse in functions
#' @name {pkg}-options-params
#' @eval zephyr::list_options(as = "params", .envir = "{pkg}")
#' @details
#' See [{pkg}-options] for more information.
#' @keywords internal
NULL

zephyr::create_option(
  name = "verbosity_level",
  default = NA_character_,
  desc = "Verbosity level for functions in {pkg}. See [zephyr::verbosity_level] for details." # nolint: line_length_linter
)

# nolint start
# To add more descriptions:
# zephyr::create_option(
#   name = "demo",
#   default = "default",
#   desc = "my description"
# )
# nolint end
