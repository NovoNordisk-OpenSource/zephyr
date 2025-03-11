run_output <- function(func, extra_lib, args = list()) {
  libpath <- c(extra_lib, .libPaths())
  callr::r(
    func = func,
    args = args,
    show = TRUE,
    libpath = libpath,
    spinner = FALSE
  )
}

run_output_project <- function(func, extra_lib, project) {
  libpath <- c(extra_lib, .libPaths())
  callr::r(
    func = \(project, func) {
      usethis::with_project(path = project, code = func(), quiet = TRUE)
    },
    args = list(project = project, func = func),
    show = TRUE,
    libpath = libpath,
    spinner = FALSE
  )
}

libpath <- withr::local_tempdir()

test_that("integration in new package", {
  skip_on_cran()
  skip_on_covr()
  skip_if_not_installed("withr")
  skip_if_not_installed("usethis")
  skip_if_not_installed("devtools")

  # Settings

  rlang::local_interactive(FALSE)
  withr::local_options(usethis.quiet = TRUE)

  # Temp folders

  testpkg <- withr::local_tempdir() |>
    file.path("testpkg")
  dir.create(testpkg)

  # Create package and copy stuff

  usethis::create_package(path = testpkg, rstudio = FALSE)
  file.copy(
    from = test_path("scripts", "greet.R"),
    to = file.path(testpkg, "R")
  )

  # Install zephyr in tmp libpath
  pkg <- file.path(test_path(), "../..")
  if (file.exists(file.path(pkg, "DESCRIPTION"))) {
    pkg <- "."
  } else {
    skip_on_os("windows")
    pkg <- file.path(pkg, "zephyr") |>
      normalizePath()
  }

  run_output(
    \(p) devtools::install( # nolint: brace_linter
      pkg = p,
      quiet = TRUE,
      quick = TRUE
    ),
    libpath,
    list(p = pkg)
  ) |>
    expect_true()

  # Use in new package

  run_output_project(\() zephyr::use_zephyr(), libpath, testpkg) |>
    expect_true() |>
    expect_snapshot()

  # Only to not get warnings below
  run_output_project(\() usethis::use_mit_license(), libpath, testpkg)

  run_output_project(\() devtools::document(quiet = TRUE), libpath, testpkg) |>
    expect_snapshot()

  run_output_project(
    \() devtools::install(dependencies = FALSE, quiet = TRUE),
    libpath,
    testpkg
  ) |>
    expect_true()
})

test_that("use in new package", {
  skip_on_cran()
  skip_on_covr()
  skip_if_not_installed("withr")
  skip_if_not_installed("usethis")
  skip_if_not_installed("devtools")
  skip_if_not_installed("callr")
  skip_if(
    condition = !file.exists(file.path(libpath, "testpkg")),
    message = "testpkg not installed - expected if test above has been skipped"
  )

  run_output(\() testpkg::greet("there"), libpath) |>
    expect_output("hello there")

  run_output(
    \() withr::with_envvar( # nolint: brace_linter
      list(R_TESTPKG_GREETING = "hej"),
      testpkg::greet("there")
    ),
    libpath
  ) |>
    expect_output("hej there")

  run_output(
    \() withr::with_options( # nolint: brace_linter
      list(testpkg.greeting = "hi"),
      testpkg::greet("there")
    ),
    libpath
  ) |>
    expect_output("hi there")

  run_output(\() zephyr::get_verbosity_level(.envir = "testpkg"), libpath) |>
    expect_equal("verbose")

  run_output(
    \() withr::with_options( # nolint: brace_linter
      list(zephyr.verbosity_level = "minimal"),
      zephyr::get_verbosity_level(.envir = "testpkg")
    ),
    libpath
  ) |>
    expect_equal("minimal")

  run_output(
    \() withr::with_options( # nolint: brace_linter
      list(testpkg.verbosity_level = "debug"),
      zephyr::get_verbosity_level(.envir = "testpkg")
    ),
    libpath
  ) |>
    expect_equal("debug")

  run_output(
    \() withr::with_options( # nolint: brace_linter
      list(zephyr.verbosity_level = "minimal"),
      testpkg::greet("there")
    ),
    libpath
  ) |>
    expect_output(NA)

  run_output(
    \() withr::with_options( # nolint: brace_linter
      list(testpkg.verbosity_level = "quiet"),
      testpkg::greet("there")
    ),
    libpath
  ) |>
    expect_output(NA)
})
