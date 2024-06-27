
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zephyr

<!-- badges: start -->

[![R-CMD-check](https://github.com/NN-OpenSource/zephyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NN-OpenSource/zephyr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The zephyr package provides small functionalities for developers of R
packages to inform users while allowing them to easily configure the
amount of information they want to receive through package level
options.

You’ve probably encountered arguments like `verbose` or `silent` in
miscellaneous functions. These arguments are used to control whether to
print information to the console, where the developer of the function
has usually done a logical check inside their function, and if `TRUE`,
they print information to the console. For the developer, doing these
logical checks explicitly each time information should be printed (or
not) can create a lot of boilerplate code. For users, they need to set
the argument in the function call each time they want to change the
default behaviour, and they often only have the option of whether to get
information or not.

The zephyr package provides tools for making this easier for both
developers and end users. This is done by providing functionalities for
writing messages that automatically performs the logical check based on
the verbosity level set in the package options. The verbosity level can
be set at the package level, and the user can easily configure the
verbosity level by setting options of environmental variables either on
a package level with prefix `packagename.`, or globally alter the
behavior of functions from zephyr by setting options of environmental
variables with the prefix `zephyr.`.

<!-- Include content from zephyr.Rmd file -->
