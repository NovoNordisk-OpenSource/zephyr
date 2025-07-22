test_that("spinner - function", {
  expect_null(
    spinner(
      x = function() Sys.sleep(0.54),
      msg = NULL
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


test_that("with_spinner handles warnings correctly", {
  expect_warning(
    {
      Sys.sleep(1)
      warning()
    } |>
      with_spinner()
  )
})

test_that("with_spinner zephyr messaging", {
  output <- capture.output(
    {
      one <- {
        Sys.sleep(1)
        print('hello')
        1
      } |>
        with_spinner()
    },
    type = "message"
  )
  if (interactive()) {
    expect_equal(nchar(output), 13)
  }
  expect_equal(1, one)
})

test_that("spinner handles successful execution", {
  result <- {
    Sys.sleep(1)
    1 + 1
  } |>
    with_spinner()

  expect_equal(result, 2)
})

test_that("spinner handles errors correctly", {
  expect_error(
    {
      Sys.sleep(1)
      stop()
    } |>
      with_spinner()
  )
})


test_that('with_spinner - no msg', {
  one <- {
    #print("This is a very long story, so that the code executed doesnt fit")
    Sys.sleep(1)
    Sys.sleep(1)

    1
  } |>
    with_spinner()
  expect_equal(one, 1)
})


test_that("with_spinner - various expressions", {
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


test_that("start_spinner and stop_spinner", {
  ctx <- start_spinner("Manual spinner running: ")
  result <- "success"
  Sys.sleep(0.5)
  stop_spinner(ctx)
  expect_equal(result, "success")
})

test_that("spinner errors when not given a function", {
  expect_error(spinner(123), "Please pass a function")
  expect_error(spinner(NULL), "Please pass a function")
})

test_that("stop_spinner returns invisibly and does nothing when ctx is NULL", {
  expect_invisible(stop_spinner(NULL))
})

test_that("stop_spinner sends correct error status", {
  fake_mq <- list(
    send = function(msg) assign("last_msg", msg, envir = parent.frame(2)),
    remove = function() NULL
  )
  fake_sem <- list(wait = function(timeout_ms) TRUE, remove = function() NULL)
  fake_process <- list(is_alive = function() TRUE, kill = function() {
    assign("killed", TRUE, envir = parent.frame(2))
  })

  killed <- FALSE
  last_msg <- NULL

  ctx <- list(
    msg = "Test Job",
    mq = fake_mq,
    sem = fake_sem,
    process = fake_process
  )

  expect_invisible(stop_spinner(ctx, status = "Failed!", error = TRUE))
  expect_equal(last_msg, "[ERR] Test Job: Failed! ")
  expect_true(killed)
})


test_that("stop_spinner returns invisibly and does nothing when ctx is NULL", {
  expect_invisible(stop_spinner(NULL))
})


test_that(".spinner_worker handles STOP message correctly", {
  sem_posts <- 0
  fake_mq <- list(receive = function(timeout_ms = 100) "STOP")
  class(fake_mq) <- "fake_mq"
  fake_sem <- list(post = function() sem_posts <<- sem_posts + 1)
  class(fake_sem) <- "fake_sem"
  mock_msg_queue <- function(...) fake_mq
  mock_semaphore <- function(...) fake_sem

  output <- capture.output(
    with_mocked_bindings(
      msg_queue = mock_msg_queue,
      semaphore = mock_semaphore,
      .package = "interprocess",
      zephyr:::.spinner_worker("testid", "Test spinner")
    )
  )

  expect_gte(sem_posts, 1, "post() not called on semaphore")
})
