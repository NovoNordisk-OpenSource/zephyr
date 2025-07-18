test_that("spinner - function", {
  skip_on_ci()
  expect_null(
    spinner(
      x = function() Sys.sleep(0.54),
      msg = "testing: spinner works"
    ),
    info = "Should return NULL when process completes successfully"
  )
})

test_that("spinner - function fails properly in error", {
  skip_on_ci()
  expect_error(
    spinner(
      x = function() stop("Error"),
      msg = "testing: spinner fails"
    ),
    regexp = "Error",
    info = "Should propagate error from background process"
  )
})

test_that("with_spinner - various expressions", {
  skip_on_ci()
  cases <- list(
    list(
      env = list(
        catalog_name = "test_catalog",
        schema_name = "test_schema",
        volume_name = "test_volume"
      ),
      expr = quote(paste(catalog_name, schema_name, volume_name, sep = ":")),
      expected = "test_catalog:test_schema:test_volume",
      msg = "Testing env vars"
    ),
    list(
      env = list(outer_var = "outer", inner_var = "inner"),
      expr = quote(paste(outer_var, inner_var, sep = "-")),
      expected = "outer-inner",
      msg = "Testing nested env vars"
    ),
    list(
      env = list(arg1 = "value1", arg2 = "value2"),
      expr = quote(paste(arg1, arg2, sep = "+")),
      expected = "value1+value2",
      msg = "Testing with args"
    )
  )

  for (case in cases) {
    list2env(case$env, envir = environment())
    result <- with_spinner(eval(case$expr), msg = case$msg)
    expect_equal(result, case$expected)
  }
})


test_that("start and stop spinner", {
  skip_on_ci()
  ctx <- start_spinner("Manual spinner running: ")
  Sys.sleep(0.5)
  result <- "success"
  Sys.sleep(0.5)
  stop_spinner(ctx)

  expect_equal(result, "success")
})
