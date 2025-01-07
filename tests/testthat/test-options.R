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

  withr::local_envvar(list(R_TESTENV_TEST_OPTION = "200"))

  get_option("test_option", .envir = testenv) |>
    expect_equal(200, info = "Should prioritize environment variable")

  withr::local_options(list(testenv.test_option = 100))

  get_option("test_option", .envir = testenv) |>
    expect_equal(100, info = "Should prioritize option")

})

test_that("list_options", {

  testenv <- simulate_package_env("testenv")
  create_option(name = "test_option", default = 42, .envir = testenv)

  list_options(.envir = testenv) |>
    expect_s3_class("zephyr_options") |>
    expect_length(2) |>
    print() |>
    expect_snapshot()

  list_options(as = "params", .envir = testenv) |>
    expect_type("character") |>
    expect_length(2) |>
    print() |>
    expect_snapshot()

  list_options(as = "markdown", .envir = testenv) |>
    expect_type("character") |>
    expect_length(1) |>
    cat() |>
    expect_snapshot()
})
