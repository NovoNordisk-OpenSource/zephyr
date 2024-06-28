# Create an environment and define a test option in it
foo_pkg <- create_env_with_fun("foo",
                               default = "test")

test_that("Option can be extracted", {
  opt <- get_verbosity_level(env = foo_pkg)

  expect_equal(opt, "test")
})

test_that("Option can be overwritten with global option or envvar", {
  glob_opt_list <- list(zephyr.verbosity_level = "glob_opt",
                        foo_pkg.verbosity_level = "test_opt")

  opt <- withr::with_options(glob_opt_list, {
    get_verbosity_level(env = foo_pkg)
  })
  expect_equal(opt, "glob_opt")

  glob_env_list <- list(R_ZEPHYR_VERBOSITY_LEVEL = "glob_opt",
                        R_FOO_PKG_VERBOSITY_LEVEL = "test_opt")

  opt2 <- withr::with_envvar(glob_env_list, {
    get_verbosity_level(env = foo_pkg)
  })
  expect_equal(opt2, "glob_opt")

  opt3 <- withr::with_envvar(list(R_ZEPHYR_VERBOSITY_LEVEL = "glob_opt"), {
    withr::with_options(list(foo_pkg.verbosity_level = "test_opt"), {
      get_verbosity_level(env = foo_pkg)
    })
  })
  expect_equal(opt3, "glob_opt")
})
