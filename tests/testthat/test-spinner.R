test_that("spinner - function", {
  expect_null(
    spinner(
      x = function() Sys.sleep(0.54),
      msg = "testing: spinner works"
    ),
    info = "Should return NULL when process completes successfully"
  )
})

test_that("spinner - function fails properly in error", {
  expect_error(
    spinner(
      x = function() stop("Error"),
      msg = "testing: spinner fails"
    ),
    regexp = "Error",
    info = "Should propagate error from background process"
  )
})

test_that("with_spinner - expression", {
  skip_on_cran()

  test_env_vars <- function() {
    catalog_name <- "test_catalog"
    schema_name <- "test_schema"
    volume_name <- "test_volume"

    result <- with_spinner(
      {
        paste(catalog_name, schema_name, volume_name, sep = ":")
      },
      msg = "Testing env vars"
    )

    expect_equal(result, "test_catalog:test_schema:test_volume")
  }

  test_env_vars()

  outer_var <- "outer"
  test_nested_env <- function() {
    inner_var <- "inner"

    result <- with_spinner(
      {
        paste(outer_var, inner_var, sep = "-")
      },
      msg = "Testing nested env vars"
    )

    expect_equal(result, "outer-inner")
  }

  test_nested_env()
})

test_that("with_spinner - expression", {
  # Test that directly uses function arguments
  test_with_args <- function(arg1, arg2) {
    result <- with_spinner(
      {
        paste(arg1, arg2, sep = "+")
      },
      msg = "Testing with args"
    )

    expect_equal(result, "value1+value2")
  }

  # Run the test with arguments
  test_with_args("value1", "value2")
})
