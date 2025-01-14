test_that("use_zephyr", {
  rlang::local_interactive(FALSE)
  withr::local_options(usethis.quiet = TRUE)

  mypkg <- withr::local_tempdir() |>
    file.path("mypkg")

  dir.create(mypkg)

  usethis::create_package(path = mypkg, rstudio = FALSE) |>
    capture.output()

  usethis::with_project(mypkg, {
    use_zephyr() |>
      expect_invisible() |>
      expect_true() |>
      expect_snapshot()

    file.exists("R/mypkg-options.R") |>
      expect_true()
  })
})
