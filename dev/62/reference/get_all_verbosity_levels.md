# Get all verbosity levels

Retrieves all active verbosity levels set for loaded packages.

See also
[verbosity_level](https://novonordisk-opensource.github.io/zephyr/reference/verbosity_level.md)
and
[`get_verbosity_level()`](https://novonordisk-opensource.github.io/zephyr/reference/get_verbosity_level.md).

## Usage

``` r
get_all_verbosity_levels()
```

## Value

Named `[character()]` vector with package as names and their verbosity
levels as values.

## Examples

``` r
get_all_verbosity_levels()
#>    zephyr 
#> "verbose" 
```
