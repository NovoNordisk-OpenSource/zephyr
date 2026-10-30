# List package options

List all `zephyr_options` specified in a package. Either as an `list` or
as as `character` vector formatted for use in your package
documentation.

To document your options use
[`use_zephyr()`](https://novonordisk-opensource.github.io/zephyr/reference/use_zephyr.md)
to set everything up, and edit the created template as necessary.

## Usage

``` r
list_options(
  as = c("list", "params", "markdown"),
  .envir = sys.function(which = -1)
)
```

## Arguments

- as:

  `[character(1)]` Format in which to return the options:

  - `"list"`: Return a nested list, where each top level element is a
    list with the specification of an option.

  - `"params"`: Return a character vector with the `"@param"` tag
    entries for each option similar to how function parameters are
    documented with roxygen2.

  - `"markdown"`: Return a character string with markdown formatted
    entries for each option.

- .envir:

  Environment in which the options are defined. Default is suitable for
  use inside your package.

## Value

`list` or `character` depending on `as`

## Examples

``` r
# List all options in zephyr
x <- list_options(.envir = "zephyr")
print(x)
#> 
#> ── verbosity_level 
#> test
#> • Default: `"verbose"`
#> • Option: `zephyr.verbosity_level`
#> • Environment: `R_ZEPHYR_VERBOSITY_LEVEL`
str(x)
#> List of 1
#>  $ verbosity_level:List of 4
#>   ..$ default    : chr "verbose"
#>   ..$ name       : chr "verbosity_level"
#>   ..$ description: chr "test"
#>   ..$ environment: chr "zephyr"
#>   ..- attr(*, "class")= chr "zephyr_option"
#>  - attr(*, "class")= chr "zephyr_options"

# Create @params tag entries for each option
list_options(as = "params", .envir = "zephyr") |>
  cat()
#> @param verbosity_level test. Default: `"verbose"`.

# List options in markdown format
list_options(as = "markdown", .envir = "zephyr") |>
  cat()
#> ## verbosity_level
#> test
#> * Default: `"verbose"`
#> * Option: `zephyr.verbosity_level`
#> * Environment: `R_ZEPHYR_VERBOSITY_LEVEL`
```
