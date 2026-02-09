# Changelog

## zephyr (development version)

- Added
  [`set_option()`](https://novonordisk-opensource.github.io/zephyr/reference/set_option.md)
  for setting package options programmatically.
- Added
  [`set_verbosity_level()`](https://novonordisk-opensource.github.io/zephyr/reference/set_verbosity_level.md)
  as a convenience wrapper for setting verbosity level.

## zephyr 0.1.3

CRAN release: 2025-08-22

- Added new default argument to
  [`get_option()`](https://novonordisk-opensource.github.io/zephyr/reference/get_option.md).
- Added upload of coverage results to codecov.io.

## zephyr 0.1.2

CRAN release: 2025-03-13

- Fixes a bug in
  [`msg()`](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
  where the verbosity level of the package using zephyr was not
  respected.

## zephyr 0.1.1

CRAN release: 2025-01-28

- Fixes a bug where
  [`list_options()`](https://novonordisk-opensource.github.io/zephyr/reference/list_options.md)
  was being able to document options with non vector default values, or
  with length different from one.
- [`get_option()`](https://novonordisk-opensource.github.io/zephyr/reference/get_option.md)
  now gives consistent return for non vector options, e.g. functions.

## zephyr 0.1.0

CRAN release: 2025-01-17

- Initial release to CRAN.
- Provides a structured framework for consistent user communication and
  configuration management for package developers.
