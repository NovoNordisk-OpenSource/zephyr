# Package index

## Messages and verbosity levels

Functions to provide consistent messages to users across our packages,
including the handling of different levels of verbosity, and utility
functions to retrieve the current verbosity level.

- [`msg()`](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
  [`msg_verbose()`](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
  [`msg_debug()`](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
  [`msg_success()`](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
  [`msg_danger()`](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
  [`msg_warning()`](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
  [`msg_info()`](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
  : Write messages based on verbosity level
- [`verbosity_level`](https://novonordisk-opensource.github.io/zephyr/reference/verbosity_level.md)
  : Verbosity level to control package behavior
- [`get_verbosity_level()`](https://novonordisk-opensource.github.io/zephyr/reference/get_verbosity_level.md)
  : Get verbosity level
- [`set_verbosity_level()`](https://novonordisk-opensource.github.io/zephyr/reference/set_verbosity_level.md)
  : Set verbosity level
- [`get_all_verbosity_levels()`](https://novonordisk-opensource.github.io/zephyr/reference/get_all_verbosity_levels.md)
  : Get all verbosity levels

## Options

Functions to create and use package specific options. This framework is
heavily inspired by the
[options](https://cran.r-project.org/package=options) package. For
packages outside our ecosystem, we recommend using options instead for a
more complete implementation.

- [`use_zephyr()`](https://novonordisk-opensource.github.io/zephyr/reference/use_zephyr.md)
  : Use zephyr options and verbosity levels
- [`create_option()`](https://novonordisk-opensource.github.io/zephyr/reference/create_option.md)
  : Create package option
- [`get_option()`](https://novonordisk-opensource.github.io/zephyr/reference/get_option.md)
  : Get value of package option
- [`set_option()`](https://novonordisk-opensource.github.io/zephyr/reference/set_option.md)
  : Set value of package option
- [`list_options()`](https://novonordisk-opensource.github.io/zephyr/reference/list_options.md)
  : List package options

## Utilities

Other utility functions used across our packages.

- [`report_checkmate_assertions()`](https://novonordisk-opensource.github.io/zephyr/reference/report_checkmate_assertions.md)
  : Report collection of assertions
