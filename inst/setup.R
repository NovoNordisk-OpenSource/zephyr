#' @title Options for {pkg}
#' @name {pkg}-options
#' @eval zephyr::as_roxygen_docs_pkg("{pkg}")
NULL

#' @title Internal parameters for reuse in functions
#' @name {pkg}-options-params
#' @eval zephyr::as_params_pkg("{pkg}")
#' @keywords internal
NULL

zephyr::define_option_pkg(
  option = "greeting",
  default = "Hi",
  desc = "Which greeting to use",
  print_spec = FALSE
)
