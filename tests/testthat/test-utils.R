test_that("envname", {
  envname(zephyr::get_verbosity_level) |>
    expect_equal("zephyr")

  envname(1) |>
    expect_error()
})

test_that("sys_getenv", {
  sys_getenv("fake_env") |>
    expect_null()

  withr::with_envvar(
    new = list(myenv = "myvalue"),
    code = {sys_getenv("myenv")}
    )|>
    expect_equal("myvalue")

  withr::with_envvar(
    new = list(myenv = "value1;value2;value3"),
    code = {sys_getenv("myenv")}
  )|>
    expect_equal(c("value1", "value2", "value3"))
})

test_that("fix_env_class", {
  fix_env_class(x = "a") |>
    expect_equal("a")

  fix_env_class(x = NULL) |>
    expect_null()

  fix_env_class(x = NULL, to_class = "numeric") |>
    expect_null()

  fix_env_class(x = c("YES", "y", "t", "TRUE", "true"), to_class = "logical") |>
    all() |>
    expect_true()

  fix_env_class(x = c("No", "n", "f", "FALSE", "false"), to_class = "logical") |>
    all() |>
    expect_false()

  fix_env_class(x = c("1", "2", "3", "4", "5"), to_class = "numeric") |>
    expect_equal(1:5)

  fix_env_class(x = "a", to_class = numeric) |>
    expect_error()

  fix_env_class(x = c("T", "false", "a"), to_class = "logical") |>
    expect_equal(c(TRUE, FALSE, NA))
})

test_that("coalesce_dots", {
  coalesce_dots(1, 2, 3) |>
    expect_equal(1)

  coalesce_dots(NA, 2, 3) |>
    expect_equal(2)

  coalesce_dots(NA, NA, 3) |>
    expect_equal(3)

  coalesce_dots(NA, NA, NA) |>
    expect_null()

  coalesce_dots(NULL, NA, mtcars) |>
    expect_equal(mtcars)
})
