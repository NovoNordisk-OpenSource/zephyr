# Set verbosity level

Sets the verbosity level for a package. This is a convenience wrapper
around
[`set_option()`](https://novonordisk-opensource.github.io/zephyr/reference/set_option.md)
for the `verbosity_level` option.

## Usage

``` r
set_verbosity_level(
  level = c("quiet", "minimal", "verbose", "debug"),
  .envir = "zephyr"
)
```

## Arguments

- level:

  `[character(1)]` Verbosity level to set. Must be one of `"quiet"`,
  `"minimal"`, `"verbose"`, or `"debug"`.

- .envir:

  Environment in which the options are defined. Default `zephyr` is
  suitable to affect all packages using zephyr.

## Value

Invisible `[character(1)]` of the previous verbosity level.

## Examples

``` r
if (FALSE) {
# Set verbosity level to minimal for zephyr
set_verbosity_level("minimal")
}
```
