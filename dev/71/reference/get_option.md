# Get value of package option

Retrieves the value of an `zephyr_option`. The value is looked up in the
following order:

1.  User defined option: `{pkgname}.{name}`

2.  System variable: `R_{PKGNAME}_{NAME}`

3.  Value of `default` argument (if not `NULL`)

4.  Default value defined with
    [`create_option()`](https://novonordisk-opensource.github.io/zephyr/reference/create_option.md)

And returns the first set value.

## Usage

``` r
get_option(name, .envir = sys.function(which = -1), default = NULL)
```

## Arguments

- name:

  `[character(1)]` Name of the option

- .envir:

  Environment in which the option is defined. Default is suitable for
  use inside your package.

- default:

  default value to return if the option is not set. If `NULL` uses the
  default set with
  [`create_option()`](https://novonordisk-opensource.github.io/zephyr/reference/create_option.md).

## Value

Value of the option

## Details

Environment variables are always defined as character strings. In order
to return consistent values the following conversions are applied:

1.  If they contain a ";" they are split into a vector using ";" as the
    delimiter.

2.  If the class of the default value is not character, the value is
    converted to the same class using the naive `as.{class}` function.
    E.g. conversions to numeric are done with
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html).

These conversions are simple in nature and will not cover advanced
cases, but we should try to keep our options this simple.

## Examples

``` r
# Retrieve the verbosity level option set by default in zephyr:
get_option(name = "verbosity_level", .envir = "zephyr")
#> [1] "verbose"

# Try to retrieve an unset option, which will return the default value:
get_option(
  name = "my_unset_option",
  .envir = "mypkg",
  default = "my_default_value"
)
#> [1] "my_default_value"
```
