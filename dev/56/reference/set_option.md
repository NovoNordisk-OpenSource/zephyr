# Set value of package option

Sets the value of a zephyr option. This is a convenience wrapper around
[`options()`](https://rdrr.io/r/base/options.html) that handles the
option naming convention.

The option is set as `{pkgname}.{name}` (lowercase).

## Usage

``` r
set_option(name, value, .envir = parent.frame())
```

## Arguments

- name:

  `[character(1)]` Name of the option

- value:

  Value to set for the option. Use `NULL` to unset previously set R
  option.

- .envir:

  Environment in which the option is defined. Default is suitable for
  use inside your package.

## Value

Invisible previous value of the option (from
[`get_option()`](https://novonordisk-opensource.github.io/zephyr/reference/get_option.md))

## Examples

``` r
if (FALSE) {
# example code
set_option(name = "my_option", value = "my_value", .envir = "my_pkg")
}
```
