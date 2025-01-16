test_that("simple verbosity level", {
  get_verbosity_level() |>
    expect_equal("verbose")

  skip_if_not_installed("withr")

  withr::with_options(
    list(zephyr.verbosity_level = "invalid"),
    {
      get_verbosity_level() |>
        expect_message()

      get_verbosity_level() |>
        suppressMessages() |>
        expect_equal("verbose")
    }
  )

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

test_that("verbosity level respects priority hierarchy when used in package", {
  # Test setup
  test_env <- simulate_package_env("testpkg")

  # Test 1: Default value
  expect_equal(get_verbosity_level(test_env), "verbose")

  # Test 2: Package-specific option (highest priority)
  skip_if_not_installed("withr")

  withr::with_options(list(testpkg.verbosity_level = "debug"), {
    expect_equal(get_verbosity_level(test_env), "debug")
  })

  # Test 3: Package-specific environment variable
  withr::with_envvar(list(R_TESTPKG_VERBOSITY_LEVEL = "minimal"), {
    expect_equal(get_verbosity_level(test_env), "minimal")
  })

  # Test 4: Global Zephyr package option
  withr::with_options(list(zephyr.verbosity_level = "quiet"), {
    expect_equal(get_verbosity_level(test_env), "quiet")
  })

  # Test 5: Zephyr environment variable
  withr::with_envvar(list(R_ZEPHYR_VERBOSITY_LEVEL = "debug"), {
    expect_equal(get_verbosity_level(test_env), "debug")
  })

  # Test 6: Priority order (1 > 2)
  withr::with_options(list(testpkg.verbosity_level = "debug"), {
    withr::with_envvar(list(R_TESTPKG_VERBOSITY_LEVEL = "minimal"), {
      expect_equal(get_verbosity_level(test_env), "debug")
    })
  })

  # Test 7: Priority order (2 > 3)
  withr::with_envvar(list(R_TESTPKG_VERBOSITY_LEVEL = "minimal"), {
    withr::with_options(list(zephyr.verbosity_level = "quiet"), {
      expect_equal(get_verbosity_level(test_env), "minimal")
    })
  })

  # Test 8: Priority order (3 > 4)
  withr::with_options(list(zephyr.verbosity_level = "quiet"), {
    withr::with_envvar(list(R_ZEPHYR_VERBOSITY_LEVEL = "debug"), {
      expect_equal(get_verbosity_level(test_env), "quiet")
    })
  })

  # Test 9: Full priority chain

  withr::with_options(
    list(
      zephyr.verbosity_level = "quiet",
      testpkg.verbosity_level = "debug"
    ),
    withr::with_envvar(
      list(
        R_ZEPHYR_VERBOSITY_LEVEL = "verbose",
        R_TESTPKG_VERBOSITY_LEVEL = "minimal"
      ),
      {
        expect_equal(get_verbosity_level(test_env), "debug")
      }
    )
  )
})

test_that("get_all_verbosity_levels", {
  get_all_verbosity_levels() |>
    expect_equal(c(zephyr = "verbose"))

  skip_if_not_installed("withr")

  withr::with_namespace(c("cli", "glue"), {
    withr::local_options(list(
      zephyr.verbosity_level = "quiet",
      cli.verbosity_level = "minimal",
      glue.verbosity_level = "debug"
    ))
    get_all_verbosity_levels() |>
      expect_mapequal(
        c(zephyr = "quiet", cli = "minimal", glue = "debug")
      )
  })
})
