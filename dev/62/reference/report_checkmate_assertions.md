# Report collection of assertions

Improved reporting of an `AssertCollection` created with the
[`checkmate::makeAssertCollection()`](https://mllg.github.io/checkmate/reference/AssertCollection.html)
using
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
instead of
[`checkmate::reportAssertions()`](https://mllg.github.io/checkmate/reference/AssertCollection.html)
in order to provide a more informative error message.

The function is intended to be used inside a function that performs
assertions on its input arguments.

## Usage

``` r
report_checkmate_assertions(
  collection,
  message = "Invalid input(s):",
  .envir = parent.frame()
)
```

## Arguments

- collection:

  `[AssertCollection]` A collection of assertions created with
  [`checkmate::makeAssertCollection()`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

- message:

  `[character(1)]` string with the header of the error message if any
  assertions failed

- .envir:

  The `[environment]` to use for the error message. Default
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) will be
  sufficient for most use cases.

## Value

`invisible(TRUE)`

## Examples

``` r
add_numbers <- function(a, b) {
  collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(x = a, add = collection)
  checkmate::assert_numeric(x = b, add = collection)
  report_checkmate_assertions(collection)
  return(a + b)
}

add_numbers(1, 2)
#> [1] 3
try(add_numbers(1, "b"))
#> Error in add_numbers(1, "b") : Invalid input(s):
#> ✖ Variable 'b': Must be of type 'numeric', not 'character'.
try(add_numbers("a", "b"))
#> Error in add_numbers("a", "b") : Invalid input(s):
#> ✖ Variable 'a': Must be of type 'numeric', not 'character'.
#> ✖ Variable 'b': Must be of type 'numeric', not 'character'.
```
