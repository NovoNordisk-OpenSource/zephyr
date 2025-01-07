test_that("use_zephyr", {
  rlang::local_interactive(FALSE)

  mypkg <- withr::local_tempdir() |>
    file.path("mypkg")

  dir.create(mypkg)

  usethis::create_package(path = mypkg, rstudio = FALSE) |>
    capture.output()

  usethis::with_project(mypkg, {
    use_zephyr() |>
      expect_snapshot()

    file.exists("R/mypkg-options.R") |>
      expect_true()

    desc::desc_has_dep(package = "zephyr", type = "Imports") |>
      expect_true()

    devtools::document() |>
      expect_snapshot() # Expect only the same warnings
    }
  )

})

