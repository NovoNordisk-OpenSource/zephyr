test_that("msg functions are muted with verbosity_level = 'quiet'", {
  expect_no_message(msg("test", verbosity_level = "quiet"))
  expect_no_message(msg_success("test", verbosity_level = "quiet"))
  expect_no_message(msg_debug("test", verbosity_level = "quiet"))
})

test_that("msg_debug is mutes when verbosity_level = 'verbose'", {
  expect_message(msg("test", verbosity_level = "verbose"),
                 "test")
  expect_message(msg_success("test", verbosity_level = "verbose"),
                 "test")
  expect_no_message(msg_debug("test", verbosity_level = "verbose"))
})

test_that("msg functions write messages with verbosity_level = 'debug'", {
  expect_message(msg("test", verbosity_level = "debug"),
                 "test")
  expect_message(msg_success("test", verbosity_level = "debug"),
                 "test")
  expect_message(msg_debug("test", verbosity_level = "debug"),
                 "test")
})

#####
# Using options to set verbosity level when used inside function in another
# environment

# Using helper function to create an environment with a function and set
# verbosity.level inside to mimic having a package foo_pkg with function foo
foo_pkg <- create_env_with_fun("foo",
                               message = "test")

test_that("verbosity_level is automatically chosen as value of option in calling function's environment", {
  withr::with_options(list(foo_pkg.verbosity_level = "quiet"), {
    expect_no_message(foo_pkg$foo())
  })

  withr::with_envvar(list(FOO_PKG_VERBOSITY_LEVEL = "quiet"), {
    expect_no_message(foo_pkg$foo())
  })

  withr::with_options(list(foo_pkg.verbosity_level = "quiet"), {
    withr::with_envvar(list(FOO_PKG_VERBOSITY_LEVEL = "verbose"), {
      expect_no_message(foo_pkg$foo())
    })
  })

  withr::with_options(list(foo_pkg.verbosity_level = "verbose"), {
    expect_message(foo_pkg$foo(), "test")
  })

  withr::with_options(list(foo_pkg.verbosity_level = "debug"), {
    expect_message(foo_pkg$foo(), "test")
  })
})


test_that("verbosity_level can be overwritten by zephyr level option", {
  withr::with_options(list(zephyr.verbosity_level = "quiet",
                           foo_pkg.verbosity_level = "verbose"), {
    expect_no_message(foo_pkg$foo())
  })
})

