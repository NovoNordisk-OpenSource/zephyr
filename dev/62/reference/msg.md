# Write messages based on verbosity level

The `msg()` function is a general utility function for writing messages
to the console based on the
[verbosity_level](https://novonordisk-opensource.github.io/zephyr/reference/verbosity_level.md)
set for your session and package.

For simple messages in your functions the recommended approach is to use
the following wrappers for consistency across packages:

- `msg_success()`: To indicate a successful operation. Wrapper around
  `msg()` using
  [`cli::cli_alert_success()`](https://cli.r-lib.org/reference/cli_alert.html)
  to display the message.

- `msg_danger()`: To indicate a failed operation. Wrapper around `msg()`
  using
  [`cli::cli_alert_danger()`](https://cli.r-lib.org/reference/cli_alert.html)
  to display the message.

- `msg_warning()`: To indicate a warning. Wrapper around `msg_verbose()`
  using
  [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html)
  to display the message.

- `msg_info()`: To provide additional information. Wrapper around
  `msg_verbose()` using
  [`cli::cli_alert_info()`](https://cli.r-lib.org/reference/cli_alert.html)
  to display the message.

For more control of how the messages are displayed use:

- `msg()`: To write messages using custom `msg_fun` functions and define
  your own verbosity levels to write.

- `msg_verbose()`: To write verbose messages with a custom `msg_fun`.

- `msg_debug()`: To to report messages only relevant when debugging.

For more information on the verbosity levels, see
[verbosity_level](https://novonordisk-opensource.github.io/zephyr/reference/verbosity_level.md).

## Usage

``` r
msg(
  message,
  levels_to_write = c("minimal", "verbose", "debug"),
  msg_fun = cli::cli_alert,
  ...,
  .envir = parent.frame()
)

msg_verbose(message, msg_fun = cli::cli_alert, ..., .envir = parent.frame())

msg_debug(message, msg_fun = cli::cli_alert, ..., .envir = parent.frame())

msg_success(message, ..., .envir = parent.frame())

msg_danger(message, ..., .envir = parent.frame())

msg_warning(message, ..., .envir = parent.frame())

msg_info(message, ..., .envir = parent.frame())
```

## Arguments

- message:

  `character` string with the text to display.

- levels_to_write:

  `character` vector with the verbosity levels for which the message
  should be displayed. Options are `minimal`, `verbose`, and `debug`.

- msg_fun:

  The function to use for writing the message. Most commonly from the
  cli package. Default is
  [`cli::cli_alert()`](https://cli.r-lib.org/reference/cli_alert.html).

- ...:

  Additional arguments to pass to `msg_fun()`

- .envir:

  The `environment` to use for evaluating the verbosity level. Default
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) will be
  sufficient for most use cases. Parsed on to `msg_fun()`.

## Value

Return from `msg_fun()`

## Examples

``` r
msg("General message")
#> → General message
msg_success("Operation successful")
#> ✔ Operation successful
msg_danger("Operation failed")
#> ✖ Operation failed
msg_warning("Warning message")
#> ! Warning message
msg_info("Additional information")
#> ℹ Additional information
```
