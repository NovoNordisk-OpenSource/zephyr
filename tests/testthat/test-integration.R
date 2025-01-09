run_output <- function(func, extra_lib) {
  libpath <- c(extra_lib, .libPaths())
  callr::r(func = func, show = TRUE, libpath = libpath, spinner = FALSE)
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

  run_output(\() devtools::install(quiet = TRUE), libpath) |>
    expect_true()

  # Use in new package

  run_output_project(\() zephyr::use_zephyr(), libpath, testpkg) |>
    expect_snapshot()

  # Only to not get warnings below
  run_output_project(\() usethis::use_mit_license(), libpath, testpkg)

  run_output_project(\() devtools::document(quiet = TRUE), libpath, testpkg) |>
    expect_snapshot()

  # run_output_project(
  #   \() rcmdcheck::rcmdcheck(quiet = TRUE, error_on = "warning"),
  #   libpath,
  #   testpkg
  # ) |>
  #   expect_no_error()

  run_output_project(
    \() devtools::install(dependencies = FALSE, quiet = TRUE),
    libpath,
    testpkg
  ) |>
    expect_true()
})

test_that("use in new package", {
  skip_on_cran()

  run_output(\() testpkg::greet("there"), libpath) |>
    expect_output("hello there")

  run_output(
    \() withr::with_envvar(
      list(R_TESTPKG_GREETING = "hej"),
      testpkg::greet("there")
    ),
    libpath
  ) |>
    expect_output("hej there")

  run_output(
    \() withr::with_options(
      list(testpkg.greeting = "hi"),
      testpkg::greet("there")
    ),
    libpath
  ) |>
    expect_output("hi there")

  run_output(\() zephyr::get_verbosity_level(.envir = "testpkg"), libpath) |>
    expect_equal("verbose")

  run_output(
    \() withr::with_options(
      list(zephyr.verbosity_level = "minimal"),
      zephyr::get_verbosity_level(.envir = "testpkg")
    ),
    libpath
  ) |>
    expect_equal("minimal")

  run_output(
    \() withr::with_options(
      list(testpkg.verbosity_level = "debug"),
      zephyr::get_verbosity_level(.envir = "testpkg")
    ),
    libpath
  ) |>
    expect_equal("debug")

  run_output(
    \() withr::with_options(
      list(zephyr.verbosity_level = "minimal"),
      testpkg::greet("there")
    ),
    libpath
  ) |>
    expect_no_condition()
})
