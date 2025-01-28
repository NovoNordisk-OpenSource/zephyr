test_that("create_option", {
  testenv <- simulate_package_env("testenv")

  create_option(name = "test_option", default = 42, .envir = testenv) |>
    expect_no_condition()

  exists(".zephyr_options", envir = testenv, inherits = FALSE) |>
    expect_true()

  testenv[[".zephyr_options"]] |>
    expect_s3_class("zephyr_options") |>
    print() |>
    expect_snapshot()

  testenv[[".zephyr_options"]][["test_option"]] |>
    expect_s3_class("zephyr_option") |>
    print() |>
    expect_snapshot()

  testenv[[".zephyr_options"]][["test_option"]] |>
    str() |>
    expect_snapshot()
})

test_that("get_option", {
  testenv <- simulate_package_env("testenv")
  create_option(name = "test_option", default = 42, .envir = testenv)

  get_option("test_option", .envir = testenv) |>
    expect_equal(42, info = "Should return default when no option is set")

  skip_if_not_installed("withr")

  withr::local_envvar(list(R_TESTENV_TEST_OPTION = "200"))

  get_option("test_option", .envir = testenv) |>
    expect_equal(200, info = "Should prioritize environment variable")

  withr::local_options(list(testenv.test_option = 100))

  get_option("test_option", .envir = testenv) |>
    expect_equal(100, info = "Should prioritize option")

  get_option(1) |>
    expect_error()

  get_option(c("a", "b")) |>
    expect_error()
})

test_that("get_option - advanced use cases", {
  testenv <- simulate_package_env("testenv")
  create_option(name = "test_null", default = NULL, .envir = testenv)
  create_option(name = "test_long", default = letters, .envir = testenv)
  create_option(name = "test_func", default = \(x) x + 1, .envir = testenv)

  get_option("test_null", .envir = testenv) |>
    expect_null()

  withr::with_options(c(testenv.test_null = 200), {
    get_option("test_null", .envir = testenv) |>
      expect_equal(200)
  })

  get_option("test_long", .envir = testenv) |>
    expect_equal(letters)

  withr::with_envvar(c(R_TESTENV_TEST_LONG = "a;b;c;d;e;f;g;h"), {
    get_option("test_long", .envir = testenv) |>
      expect_equal(c("a", "b", "c", "d", "e", "f", "g", "h"))
  })

  f <- get_option("test_func", .envir = testenv)
  expect_true(is.function(f))
  expect_equal(f(1), 2)

  withr::with_envvar(c(R_TESTENV_TEST_FUNC = "function(x) x + 2"), {
    f <- get_option("test_func", .envir = testenv)
    expect_true(is.function(f))
    expect_equal(f(1), 3)
  })

  withr::with_options(c(testenv.test_func = \(x) x + 10), {
    f <- get_option("test_func", .envir = testenv)
    expect_true(is.function(f))
    expect_equal(f(1), 11)
  })
})

test_that("zephyr options", {
  list_options(.envir = "zephyr") |>
    expect_s3_class("zephyr_options") |>
    expect_length(1) |>
    print() |>
    expect_snapshot()
})

test_that("list_options - as list", {
  testenv <- simulate_package_env("testenv")
  create_option(name = "test_option", default = 42, .envir = testenv)

  list_options(.envir = testenv) |>
    expect_s3_class("zephyr_options") |>
    expect_length(2) |>
    print() |>
    expect_snapshot()

  create_option(name = "test_null", default = NULL, .envir = testenv)
  create_option(name = "test_long", default = letters, .envir = testenv)
  create_option(name = "test_func", default = \(x) x + 1, .envir = testenv)

  list_options(.envir = testenv) |>
    expect_s3_class("zephyr_options") |>
    expect_length(5) |>
    print() |>
    expect_snapshot()
})

test_that("list_options - as params", {
  testenv <- simulate_package_env("testenv")
  create_option(name = "test_option", default = 42, .envir = testenv)

  list_options(as = "params", .envir = testenv) |>
    expect_type("character") |>
    expect_length(2) |>
    print() |>
    expect_snapshot()

  create_option(name = "test_null", default = NULL, .envir = testenv)
  create_option(name = "test_long", default = letters, .envir = testenv)
  create_option(name = "test_func", default = \(x) x + 1, .envir = testenv)

  list_options(as = "params", .envir = testenv) |>
    expect_type("character") |>
    expect_length(5) |>
    print() |>
    expect_snapshot()
})

test_that("list_options - as markdown", {
  testenv <- simulate_package_env("testenv")
  create_option(name = "test_option", default = 42, .envir = testenv)

  list_options(as = "markdown", .envir = testenv) |>
    expect_type("character") |>
    expect_length(1) |>
    cat() |>
    expect_snapshot()

  create_option(name = "test_null", default = NULL, .envir = testenv)
  create_option(name = "test_long", default = letters, .envir = testenv)
  create_option(name = "test_func", default = \(x) x + 1, .envir = testenv)

  list_options(as = "markdown", .envir = testenv) |>
    expect_type("character") |>
    expect_length(1) |>
    cat() |>
    expect_snapshot()
})
