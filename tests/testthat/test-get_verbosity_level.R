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
  remove_option_pkg("verbosity_level", envir = getNamespace("zephyr"))
}

# Helper function to simulate package environment
simulate_package_env <-  function(parent_pkg_name, new_pkg_name) {
  parent_env <- as.environment(paste0("package:", parent_pkg_name))
  pkg_env <- new.env(parent = parent_env)

  # Set the name of the new environment using attr
  attr(pkg_env, "name") <- paste0("package:", new_pkg_name)

  r_files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
  for (file in r_files) {
    sys.source(file, envir = pkg_env)
  }
  pkg_env
}

test_that("get_verbosity_level respects priority hierarchy", {
  # Test setup
  test_env <- simulate_package_env("zephyr", "testpkg")

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

  # Test 6: Package-specific option set with opt_pkg (if available)
  reset_all()
  if (exists("opt_pkg", envir = test_env)) {
    define_option_pkg("verbosity_level", "minimal", envir = "test_env", print_spec = FALSE)
    expect_equal(get_verbosity_level(test_env), "minimal")
  }

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

  # Test 10: Full priority chain
  reset_all()
  options(list(testpkg.verbosity_level = "debug"))
  Sys.setenv(R_TESTPKG_VERBOSITY_LEVEL = "minimal")
  options(zephyr.verbosity_level = "quiet")
  Sys.setenv(R_ZEPHYR_VERBOSITY_LEVEL = "verbose")
  if (exists("opt_pkg", envir = test_env)) {
    define_option_pkg("verbosity_level", "minimal", envir = "test_env", print_spec = FALSE)
  }
  expect_equal(get_verbosity_level(test_env), "debug")

  # Clean up
  reset_all()
})
