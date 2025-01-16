
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zephyr <a href="https://novonordisk-opensource.github.io/zephyr/"><img src="man/figures/logo.png" alt="zephyr website" align="right" height="138"/></a>

<!-- badges: start -->

[![Checks](https://github.com/NovoNordisk-OpenSource/zephyr/actions/workflows/check_and_co.yaml/badge.svg)](https://github.com/NovoNordisk-OpenSource/zephyr/actions/workflows/check_and_co.yaml)

<!-- badges: end -->

The zephyr package provides small functionalities for developers of R
packages to inform users about progress and issues, while at the same
time allowing the users to easily configure the amount of information
they want to receive.

zephyr also has utilities to create, get, and document options used in
your packages.

The zephyr package is designed and intended for use within other R
packages developed and published under the [Novo Nordisk Open
Source](https://novonordisk-opensource.github.io/R-packages/) ecosystem.

For packages outside our ecosystem, we recommend using the
[options](https://cran.r-project.org/package=options) package  
instead for a more complete implementation of package options.

## Installation

``` r
# Install the released version from CRAN:
install.packages("zephyr")
# Install the development version from GitHub:
pak::pak("NovoNordisk-OpenSource/zephyr")
```

## Use zephyr

The easiest way to use zephyr in your package is to set it up with the
`use_zephyr()` function.

This creates a new script `R/{pkgname}-options.R` with all the relevant
boilerplate code to use zephyr options in the package.

It adds the `verbosity_level` option, that the user can change to
configure the amount of information. For more information and the
different levels see `?verbosity_level`.

New options in your package can be added with `create_option()` and used
afterwards with `get_option()` inside your functions. Documentation is
automatically generated with `list_options()`.

## Messages and verbosity

The backbone of zephyr is the `msg()` and friends functions. They are to
be used inside your functions to give consistent levels of information
to your users.

The main functions are:

- `msg_success()`: To show the success of a long or complicated
  operation.
- `msg_danger()`: To show a failed operation. Note this is not throwing
  an error. Use `cli::cli_abort()` in that case.
- `msg_warning()`: To indicate a warning.
- `msg_info()`: To give detailed information.
- `msg_debug()`: To give debugging information.

These functions are already wrapped around the appropriate {cli}
functions and the relevant `verbosity_level`.

### Example

Let’s say we have a function that does some long and complicated
calculations, and creates some output. We want to use zephyr `msg()`
functions to inform the user of the progress and the result.

A skeleton to use for that function could be the `foo()` function below:

``` r
foo <- function() {
  msg_debug("Some important debug information about the function")
  msg_info("Starting calculations")

  result <- 1 + 1 # Replace with long calculations and how to create output

  if (result == 2) {
    msg_success("Output created")
  } else {
    msg_danger("Output not created correctly")
  }

  return(result)
}
```

Here we first provide some debugging information if requested. Then we
inform that the calculations are starting. After the calculations, we
inform the user if the output was created correctly or not.

The default `verbosity_level` in zephyr is `verbose` which displays all
messages expect debugging:

``` r
foo()
#> ℹ Starting calculations
#> ✔ Output created
#> [1] 2
```

With `verbosity_level = "minimal"` only `msg_success()` and
`msg_danger()` messages are shown:

``` r
withr::with_options(
  new = list(zephyr.verbosity_level = "minimal"),
  code = foo()
)
#> ✔ Output created
#> [1] 2
```

And they can be turned off completely with `verbosity_level = "quiet"`,
while `verbosity_level = "debug"` gives all messages:

``` r
withr::with_options(
  new = list(zephyr.verbosity_level = "quiet"),
  code = foo()
)
#> [1] 2

withr::with_options(
  new = list(zephyr.verbosity_level = "debug"),
  code = foo()
)
#> → Some important debug information about the function
#> ℹ Starting calculations
#> ✔ Output created
#> [1] 2
```
