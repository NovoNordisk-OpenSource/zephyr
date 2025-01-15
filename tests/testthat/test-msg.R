test_that("Default verbosity work as intended", {
  msg("This gives a message because default verbosity level is verbose") |>
    expect_message()

  msg_verbose("This gives a message for the same reason") |>
    expect_message()

  msg_debug("But this one does not!") |>
    expect_no_message()

  msg_success("Successes are also shown when verbosity is verbose") |>
    expect_message()

  msg_danger("And so are dangers,") |>
    expect_message()

  msg_warning("warnings,") |>
    expect_message()

  msg_info("And info messages!") |>
    expect_message()
})

test_that("Minimal verbosity has the expected behavior", {
  skip_if_not_installed("withr")
  withr::local_options(list(zephyr.verbosity_level = "minimal"))

  msg("This gives a message for everything except quiet") |>
    expect_message()

  msg_verbose("This needs to be verbose or debug") |>
    expect_no_message()

  msg_debug("And this needs debug") |>
    expect_no_message()

  msg_success("Successes are shown for minimal verbosity") |>
    expect_message()

  msg_danger("And so are dangers") |>
    expect_message()

  msg_warning("But not not warnings") |>
    expect_no_message()

  msg_info("or info messages!") |>
    expect_no_message()
})

test_that("Quiet gives no messages", {
  skip_if_not_installed("withr")
  withr::local_options(list(zephyr.verbosity_level = "quiet"))

  msg("This gives a message for everything except quiet") |>
    expect_no_message()
})
