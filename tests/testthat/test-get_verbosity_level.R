# Helper function to reset all options and environment variables
reset_all <-  function() {
  options(list(
    "testpkg.verbosity_level" = NULL,
    "zephyr.verbosity_level" = NULL
  ))
  Sys.unsetenv("R_TESTPKG_VERBOSITY_LEVEL")
  Sys.unsetenv("R_ZEPHYR_VERBOSITY_LEVEL")

  # Clear package-specific options more safely
  env <- parent.frame()
  if (exists(".options", envir = env, inherits = FALSE)) {
    if (is.environment(env$.options)) {
      rm(list = ls(envir = env$.options), envir = env$.options)
    }
  }
}

test_that("get_verbosity_level respects priority hierarchy", {
  # Helper function to set package environment
  set_package_env <- function(pkg_name) {
    env <- new.env(parent = emptyenv())
    attr(env, "name") <- paste0("package:", pkg_name)
    env
  }

  # Test setup
  test_pkg <- "testpkg"
  test_env <- set_package_env(test_pkg)

  # Test 1: Default value
  reset_all()
  expect_equal(get_verbosity_level(test_env), "verbose")

  # Test 2: Package-specific option (highest priority)
  reset_all()
  options(list(testpkg.verbosity_level = "debug"))
  expect_equal(get_verbosity_level(test_env), "debug")

  # Test 3: Package-specific environment variable
  reset_all()
  Sys.setenv(R_TESTPKG_VERBOSITY_LEVEL = "minimal")
  expect_equal(get_verbosity_level(test_env), "minimal")

  # Test 4: Global Zephyr package option
  reset_all()
  options(zephyr.verbosity_level = "quiet")
  expect_equal(get_verbosity_level(test_env), "quiet")

  # Test 5: Zephyr environment variable
  reset_all()
  Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "debug")
  expect_equal(get_verbosity_level(test_env), "debug")

  # Test 6: Package-specific option set with define_option_pkg
  reset_all()
  define_option_pkg("verbosity_level", default = "minimal", envir = test_env)
  expect_equal(get_verbosity_level(test_env), "minimal")

  # Test 7: Priority order (1 > 2)
  reset_all()
  options(list(testpkg.verbosity_level = "debug"))
  Sys.setenv(R_TESTPKG_VERBOSITY_LEVEL = "minimal")
  expect_equal(get_verbosity_level(test_env), "debug")

  # Test 8: Priority order (2 > 3)
  reset_all()
  Sys.setenv(R_TESTPKG_VERBOSITY_LEVEL = "minimal")
  options(zephyr.verbosity_level = "quiet")
  expect_equal(get_verbosity_level(test_env), "minimal")

  # Test 9: Priority order (3 > 4)
  reset_all()
  options(zephyr.verbosity_level = "quiet")
  Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "debug")
  expect_equal(get_verbosity_level(test_env), "quiet")

  # Test 10: Priority order (4 > 5)
  reset_all()
  Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "debug")
  define_option_pkg("verbosity_level", default = "minimal", envir = test_env)
  expect_equal(get_verbosity_level(test_env), "debug")

  # Test 11: Priority order (5 > 6)
  reset_all()
  define_option_pkg("verbosity_level", default = "minimal", envir = test_env)
  expect_equal(get_verbosity_level(test_env), "minimal")

  # Test 12: Full priority chain
  reset_all()
  options(list(testpkg.verbosity_level = "debug"))
  Sys.setenv(R_TESTPKG_VERBOSITY_LEVEL = "minimal")
  options(zephyr.verbosity_level = "quiet")
  Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "verbose")
  define_option_pkg("verbosity_level", default = "debug", envir = test_env)
  expect_equal(get_verbosity_level(test_env), "debug")
})
