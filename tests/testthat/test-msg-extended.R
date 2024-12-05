# Helper function to create a simulated package environment
create_pkg_env <-  function(pkg_name, fun_name, message, verbosity_level = "verbose") {
  env <- new.env()

  env$get_verbosity_level <- function() {
    level <- getOption("zephyr.verbosity_level")
    if (is.null(level)) {
      level <- getOption(paste0(pkg_name, ".verbosity_level"))
      if (is.null(level)) {
        level <- Sys.getenv(paste0("R_", toupper(pkg_name), "_VERBOSITY_LEVEL"))
        if (level == "") {
          level <- verbosity_level
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
create_foo_pkg <- function(verbosity_level = "verbose") {
  create_pkg_env("foo_pkg", "foo", "test", verbosity_level)
}

# Test fixture
verbosity_fixture <- function(env = parent.frame()) {
  withr::defer({
    options(foo_pkg.verbosity_level = NULL)
    options(zephyr.verbosity_level = NULL)
    Sys.unsetenv("R_FOO_PKG_VERBOSITY_LEVEL")
  }, envir = env)
}

# Tests
test_that("verbosity_level is correctly set and respected", {
  verbosity_fixture()

  # Test default behavior (should be "verbose")
  foo_pkg <- create_foo_pkg()
  # expect_message(foo_pkg$foo(), "test")
  # expect_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, msg_fun = cli::cli_alert_success)), "test")

  # Test with explicit "quiet" setting
  foo_pkg <- create_foo_pkg("quiet")
  expect_no_message(foo_pkg$foo())
  expect_no_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, msg_fun = cli::cli_alert_success)))

  # Test with explicit "verbose" setting
  foo_pkg <- create_foo_pkg("verbose")
  # expect_message(foo_pkg$foo(), "test")
  # expect_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, msg_fun = cli::cli_alert_success)), "test")

  # Test environment variable
  withr::with_envvar(list(R_FOO_PKG_VERBOSITY_LEVEL = "quiet"), {
    foo_pkg <- create_foo_pkg()
    expect_no_message(foo_pkg$foo())
  })

  # Test priority: package-specific option > environment variable
  withr::with_options(list(foo_pkg.verbosity_level = "quiet"), {
    withr::with_envvar(list(R_FOO_PKG_VERBOSITY_LEVEL = "verbose"), {
      foo_pkg <- create_foo_pkg()
      expect_no_message(foo_pkg$foo())
    })
  })
})

test_that("verbosity_level can be overwritten by zephyr level option", {
  verbosity_fixture()

  withr::with_options(list(
    zephyr.verbosity_level = "quiet",
    foo_pkg.verbosity_level = "verbose"
  ), {
    foo_pkg <- create_foo_pkg()
    expect_no_message(foo_pkg$foo())
  })

  for (level in c("quiet", "minimal", "verbose", "debug")) {
    withr::with_options(list(
      zephyr.verbosity_level = level,
      foo_pkg.verbosity_level = "verbose"
    ), {
      foo_pkg <- create_foo_pkg()
      if (level == "quiet") {
        expect_no_message(foo_pkg$foo())
      } else {
        expect_message(foo_pkg$foo(), "test")
      }
    })
  }
})

# Additional tests
test_that("msg functions are muted with verbosity_level = 'quiet'", {
  verbosity_fixture()
  foo_pkg <- create_foo_pkg("quiet")
  expect_no_message(foo_pkg$msg("test"))
  expect_no_message(foo_pkg$msg_success("test"))
  expect_no_message(foo_pkg$msg_debug("test"))
})

test_that("msg_debug is muted when verbosity_level = 'verbose'", {
  verbosity_fixture()
  foo_pkg <- create_foo_pkg("verbose")
  expect_message(foo_pkg$msg("test"), "test")
  expect_message(foo_pkg$msg_success("test"), "test")
  expect_no_message(foo_pkg$msg_debug("test"))
})

test_that("msg functions write messages with verbosity_level = 'debug'", {
  verbosity_fixture()
  foo_pkg <- create_foo_pkg("debug")
  expect_message(foo_pkg$msg("test"), "test")
  expect_message(foo_pkg$msg_success("test"), "test")
  expect_message(foo_pkg$msg_debug("test"), "test")
})
