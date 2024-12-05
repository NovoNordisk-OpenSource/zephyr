# Helper function to create an environment with a function and set verbosity level
create_env_with_fun <- function(fun_name, message) {
  env <- new.env()

  env$get_verbosity_level <- function() {
    level <- getOption("zephyr.verbosity_level")
    if (is.null(level)) {
      level <- getOption("foo_pkg.verbosity_level")
      if (is.null(level)) {
        level <- Sys.getenv("R_FOO_PKG_VERBOSITY_LEVEL")
        if (level == "") {
          level <- "verbose"
        }
      }
    }
    level
  }

  env$msg <- function(message, levels_to_write = c("minimal", "verbose", "debug"),
    msg_fun = cli::cli_alert_info, ...,
    verbosity_level = NULL,
    .envir = parent.frame()) {
    if (is.null(verbosity_level)) {
      verbosity_level <- env$get_verbosity_level()
    }

    if (verbosity_level %in% levels_to_write) {
      msg_fun(message, ..., .envir = .envir)
    }
  }

  env$msg_success <- function(message, ...) {
    env$msg(message, msg_fun = cli::cli_alert_success, ...)
  }

  env$msg_debug <- function(message, ...) {
    env$msg(message, levels_to_write = "debug", msg_fun = cli::cli_alert_info, ...)
  }

  env[[fun_name]] <- function(msg_fun = NULL) {
    if (is.null(msg_fun) || !is.function(msg_fun)) {
      msg_fun <- env$msg
    }
    msg_fun(message)
  }

  env
}

# Create a simulated package environment
foo_pkg <- create_env_with_fun("foo", "test")

# Tests
test_that("msg functions are muted with verbosity_level = 'quiet'", {
  expect_no_message(foo_pkg$msg("test", verbosity_level = "quiet"))
  expect_no_message(foo_pkg$msg_success("test", verbosity_level = "quiet"))
  expect_no_message(foo_pkg$msg_debug("test", verbosity_level = "quiet"))
})

test_that("msg_debug is muted when verbosity_level = 'verbose'", {
  expect_message(foo_pkg$msg("test", verbosity_level = "verbose"),
    "test")
  expect_message(foo_pkg$msg_success("test", verbosity_level = "verbose"),
    "test")
  expect_no_message(foo_pkg$msg_debug("test", verbosity_level = "verbose"))
})

test_that("msg functions write messages with verbosity_level = 'debug'", {
  expect_message(foo_pkg$msg("test", verbosity_level = "debug"),
    "test")
  expect_message(foo_pkg$msg_success("test", verbosity_level = "debug"),
    "test")
  expect_message(foo_pkg$msg_debug("test", verbosity_level = "debug"),
    "test")
})

test_that("verbosity_level is automatically chosen as value of option in calling function's environment", {
  withr::with_options(list(foo_pkg.verbosity_level = "quiet"), {
    expect_no_message(foo_pkg$foo())
  })

  withr::with_envvar(list(R_FOO_PKG_VERBOSITY_LEVEL = "quiet"), {
    expect_no_message(foo_pkg$foo())
  })

  withr::with_options(list(foo_pkg.verbosity_level = "quiet"), {
    withr::with_envvar(list(R_FOO_PKG_VERBOSITY_LEVEL = "verbose"), {
      expect_no_message(foo_pkg$foo())
    })
  })

  withr::with_options(list(foo_pkg.verbosity_level = "verbose"), {
    expect_message(foo_pkg$foo(), "test")
  })

  withr::with_options(list(foo_pkg.verbosity_level = "verbose"), {
    expect_message(foo_pkg$foo(msg_fun = foo_pkg$msg_success), "test")
  })

  withr::with_options(list(foo_pkg.verbosity_level = "verbose"), {
    expect_no_message(foo_pkg$foo(msg_fun = foo_pkg$msg_debug))
  })

  withr::with_options(list(foo_pkg.verbosity_level = "debug"), {
    expect_message(foo_pkg$foo(), "test")
  })

  withr::with_options(list(foo_pkg.verbosity_level = "debug"), {
    expect_message(foo_pkg$foo(msg_fun = foo_pkg$msg_debug), "test")
  })
})

test_that("verbosity_level can be overwritten by zephyr level option", {
  withr::with_options(list(
    zephyr.verbosity_level = "quiet",
    foo_pkg.verbosity_level = "verbose"
  ),
    {
      expect_no_message(foo_pkg$foo())
    })
})
