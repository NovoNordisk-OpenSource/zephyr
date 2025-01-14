#' Use zephyr options and verbosity levels
#' @description
#' Utility function to set up the use of zephyr options and
#' [verbosity_level] in your package.
#'
#' Creates the file `R/{pkgname}-options.R` with boiler plate code to setup
#' and document options.
#'
#' This code also creates an package specific `verbosity_level` option,
#' enabling you to control the verbosity of your package functions using
#' the [msg] functions.
#' @returns `invisible(TRUE)`
#' @examplesIf FALSE
#' use_zephyr()
#'
#' @export
use_zephyr <- function() {
  cli::cli_h1("Setting up {.pkg zephyr}")

  rlang::check_installed("usethis")

  pkgname <- basename(usethis::proj_path())

  script <- system.file("setup-options.R", package = "zephyr") |>
    readLines() |>
    vapply(
      FUN = \(x, pkg = pkgname) glue::glue(x, pkg = pkg),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )

  path <- file.path("R", paste0(pkgname, "-options.R"))

  usethis::use_package(package = "zephyr")
  usethis::write_over(path = path, lines = script)
  usethis::edit_file(path = path)

  cli::cli_alert_info("Add new options with {.code zephyr::create_option()}.")
  cli::cli_alert_info(
    "And reuse their documentation with in functions with {.code @inheritParams {pkgname}-options-params}." # nolint: line_length_linter
  )
  cli::cli_alert_info(
    "Run {.run devtools::document()} to update documentation."
  )

  return(invisible(TRUE))
}
