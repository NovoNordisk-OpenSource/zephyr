# Use zephyr options and verbosity levels

Utility function to set up the use of zephyr options and
[verbosity_level](https://novonordisk-opensource.github.io/zephyr/reference/verbosity_level.md)
in your package.

Creates the file `R/{pkgname}-options.R` with boiler plate code to setup
and document options.

This code also creates an package specific `verbosity_level` option,
enabling you to control the verbosity of your package functions using
the
[msg](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
functions.

## Usage

``` r
use_zephyr()
```

## Value

`invisible(TRUE)`

## Examples

``` r
if (FALSE) {
use_zephyr()
}
```
