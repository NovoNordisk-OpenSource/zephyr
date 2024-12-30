#' Use zephyr options and verbosity levels
#'
#' Utility function to set up the following in your package:
#'
#' 1. Use of `verbosity_level`
#' 1. Use of package specific options
#' 1. Documents all options
#'
#' @export

use_zephyr <- function() {
  cli::cli_h1("Setting up {.pkg zephyr}")

  rlang::check_installed("usethis")
  rlang::check_installed("glue")

  pkgname <- basename(usethis::proj_path())

  script <- system.file("setup.R", package = "zephyr") |>
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

  cli::cli_alert_info("Add new options with {.code zephyr::define_option_pkg()}.")
  cli::cli_alert_info("And reuse their documentation with in functions with {.code @inheritParams {pkgname}-options-params}.")
  cli::cli_alert_info("Run {.run devtools::document()} to update documentation.")
}
