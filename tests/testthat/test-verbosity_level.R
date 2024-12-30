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


test_that("verbosity level", {

  get_verbosity_level() |>
    expect_equal("verbose")

  withr::local_envvar(list(R_ZEPHYR_VERBOSITY_LEVEL = "quiet"))

  get_verbosity_level() |>
      expect_equal("quiet")

  withr::local_options(list(zephyr.verbosity_level = "minimal"))

  get_verbosity_level() |>
      expect_equal("minimal")

  get_verbosity_level("foo") |>
    expect_equal("minimal")

  withr::local_envvar(list(R_FOO_VERBOSITY_LEVEL = "debug"))

  get_verbosity_level("foo") |>
      expect_equal("debug")

  withr::local_options(list(foo.verbosity_level = "verbose"))

  get_verbosity_level("foo") |>
      expect_equal("verbose")

  get_verbosity_level() |>
    expect_equal("minimal")
})


