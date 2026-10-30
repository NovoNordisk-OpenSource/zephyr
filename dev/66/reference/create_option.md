# Create package option

Use inside your package to setup a `zephyr_option` that you can use in
your functions with
[`get_option()`](https://novonordisk-opensource.github.io/zephyr/reference/get_option.md).
The specification is stored inside the environment of your package.

For more information and how to get started see
[`use_zephyr()`](https://novonordisk-opensource.github.io/zephyr/reference/use_zephyr.md).

## Usage

``` r
create_option(name, default, description = name, .envir = parent.frame())
```

## Arguments

- name:

  `[character(1)]` Name of the option

- default:

  `[any]` Default value of the option

- description:

  `[character(1)]` Description of the option

- .envir:

  Environment in which the option is defined. Default is suitable for
  use inside your package.

## Value

Invisible `zephyr_option` object

## Examples

``` r
if (FALSE) {
create_option(
  name = "answer",
  default = 42,
  description = "This is supposed to be the question"
)
}
```
