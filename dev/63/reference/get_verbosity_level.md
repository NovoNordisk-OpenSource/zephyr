# Get verbosity level

This function retrieves the `verbosity_level` for your environment using
the priority hierarchy as described in
[verbosity_level](https://novonordisk-opensource.github.io/zephyr/reference/verbosity_level.md).

While the examples use `zephyr`, this function works with any package,
and inside a package it is not necessary to specify it; the default
value of `.envir` is enough.

It is normally not relevant to query the `verbosity_level` yourself.
Instead use the appropriate
[msg](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
function.

## Usage

``` r
get_verbosity_level(.envir = sys.function(which = -1))
```

## Arguments

- .envir:

  Environment in which the options are defined. Default is suitable for
  use inside your package.

## Value

`[character(1)]` representing the verbosity level.

## Examples

``` r
# Get the verbosity level
# Note: Specifying the environment is not needed when used inside a package
get_verbosity_level("zephyr")
#> [1] "verbose"

# Temporarily change verbosity level using an environment variable
withr::with_envvar(
  new = c("R_ZEPHYR_VERBOSITY_LEVEL" = "quiet"),
  code = get_verbosity_level("zephyr")
)
#> [1] "quiet"

# Temporarily change verbosity level using an option value
withr::with_options(
  new = c("zephyr.verbosity_level" = "minimal"),
  code = get_verbosity_level("zephyr")
)
#> [1] "minimal"
```
