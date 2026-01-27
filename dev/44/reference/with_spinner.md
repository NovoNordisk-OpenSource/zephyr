# `spinner` wrapper to avoid LHS priority eval limitations with `|>`

This function evaluates an expression while displaying a spinner
animation with a custom message.

## Usage

``` r
with_spinner(expr, msg = "Running: {.expr}")
```

## Arguments

- expr:

  The expression to evaluate

- msg:

  The message to display alongside the spinner default is
  `'Running: {.expr}'`

## Value

The result of evaluating `expr`

## Examples

``` r
#' # Simple delay with spinner
if (FALSE) {
with_spinner(Sys.sleep(2), "Waiting for 2 seconds")

# Complex expressions
with_spinner({
  Sys.sleep(1)
  Sys.sleep(1)
  "Result"
}, "Processing complex operation")
}
```
