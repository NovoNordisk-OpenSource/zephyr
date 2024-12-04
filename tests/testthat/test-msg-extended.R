# Helper function to create a simulated package environment
create_pkg_env <-  function(pkg_name, fun_name, message) {
  env <- new.env()

  # Set up a function to get the verbosity level
  env$get_verbosity_level <- function() {
    # Check for zephyr global option first
    level <- getOption("zephyr.verbosity_level")
    if (is.null(level)) {
      # Then check for package-specific option
      level <- getOption(paste0(pkg_name, ".verbosity_level"))
      if (is.null(level)) {
        # Then check for environment variable
        level <- Sys.getenv(paste0("R_", toupper(pkg_name), "_VERBOSITY_LEVEL"))
        if (level == "") {
          # Default to verbose if not set anywhere
          level <- "verbose"
        }
      }
    }
    level
  }

  # Define the msg function within the package environment
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

  # Define the main function that uses msg
  env[[fun_name]] <- function(msg_fun = env$msg) {
    msg_fun(message)
  }

  env
}

# Create a simulated package environment
foo_pkg <- create_pkg_env("foo_pkg", "foo", "test")

test_that("verbosity_level is correctly set and respected", {
  # Test with package-specific option
  withr::with_options(list(foo_pkg.verbosity_level = "quiet"), {
    expect_no_message(foo_pkg$foo())
    expect_no_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, msg_fun = cli::cli_alert_success)))
    expect_no_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, levels_to_write = "debug")))
    expect_no_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, levels_to_write = c("minimal", "verbose", "debug"))))
  })

  # Test with package-specific environment variable
  withr::with_envvar(list(R_FOO_PKG_VERBOSITY_LEVEL = "quiet"), {
    expect_no_message(foo_pkg$foo())
  })

  # Test priority: package-specific option > package-specific environment variable
  withr::with_options(list(foo_pkg.verbosity_level = "quiet"), {
    withr::with_envvar(list(R_FOO_PKG_VERBOSITY_LEVEL = "verbose"), {
      expect_no_message(foo_pkg$foo())
    })
  })

  # Test verbose level
  withr::with_options(list(foo_pkg.verbosity_level = "verbose"), {
    expect_message(foo_pkg$foo(), "test")
    expect_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, msg_fun = cli::cli_alert_success)), "test")
    expect_no_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, levels_to_write = "debug")))
  })

  # Test debug level
  withr::with_options(list(foo_pkg.verbosity_level = "debug"), {
    expect_message(foo_pkg$foo(), "test")
    expect_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, msg_fun = cli::cli_alert_success)), "test")
    expect_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, levels_to_write = "debug")), "test")
  })

  # Test minimal level
  withr::with_options(list(foo_pkg.verbosity_level = "minimal"), {
    expect_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, levels_to_write = c("minimal", "verbose", "debug"))), "test")
    expect_no_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, levels_to_write = "verbose")))
    expect_no_message(foo_pkg$foo(msg_fun = function(x) foo_pkg$msg(x, levels_to_write = "debug")))
  })
})

test_that("verbosity_level can be overwritten by zephyr level option", {
  # Test zephyr option overriding package-specific option
  withr::with_options(list(
    zephyr.verbosity_level = "quiet",
    foo_pkg.verbosity_level = "verbose"
  ), {
    expect_no_message(foo_pkg$foo())
  })

  # Test zephyr option with different levels
  for (level in c("quiet", "minimal", "verbose", "debug")) {
    withr::with_options(list(
      zephyr.verbosity_level = level,
      foo_pkg.verbosity_level = "verbose"
    ), {
      if (level == "quiet") {
        expect_no_message(foo_pkg$foo())
      } else {
        expect_message(foo_pkg$foo(), "test")
      }
    })
  }
})
