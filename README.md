
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zephyr <a href="https://novonordisk-opensource.github.io/zephyr/"><img src="man/figures/logo.png" align="right" height="138" alt="zephyr website" /></a>

<!-- badges: start -->

[![Checks](https://github.com/NovoNordisk-OpenSource/zephyr/actions/workflows/check_and_co.yaml/badge.svg)](https://github.com/NovoNordisk-OpenSource/zephyr/actions/workflows/check_and_co.yaml)
<!-- badges: end -->

The zephyr package provides small functionalities for developers of R
packages to inform users about progress and issues, while at the same
time allowing them to easily configure the amount of information they
want to receive through package level options.

You have probably encountered arguments like `verbose` or `silent` in
miscellaneous functions. These arguments are used to control whether to
print information to the console, where the developer of the function
has usually done a logical check inside their function, and if `TRUE`,
they print information to the console. For the developer, doing these
logical checks explicitly each time information should be printed (or
not) can create a lot of redundant code. For users, they need to set the
argument in the function call each time they want to change the default
behaviour, and often the users are left with the dichotomy of either
getting every piece of information or none.

The zephyr package provides tools for making this easier for both
developers and end users. This is done by providing functionalities for
writing messages that automatically performs the logical check based on
the verbosity level set in the package options. The verbosity level can
be set at the package level, and the user can easily configure the
verbosity level by setting options of environmental variables either on
a package level with prefix `packagename.`, or globally alter the
behavior of functions from zephyr by setting options of environmental
variables with the prefix `zephyr.`.

## Zephyr message functionalities

These functionalities are intented to be used by developers of R
packages. Below the basic principle of the functions are explained and
it is showed how to use the functions in the context of R package
development.

### Basic features

The backbone functions of `zephyr` are `msg`, `msg_debug` and
`msg_success` which will collectively be referred to as `msg` functions
in the remainder of this README. They have a common documentation page
that can be accessed by `?msg`. The purpose of these functions is to
write messages to the console dependent on a `verbosity_level` that can
be specified through options. Specifically, as outlined below the
message is only written to the console when the `verbosity_level` is
matching a level in `levels_to_write`.

``` r
msg("testing",
    levels_to_write = c("verbose", "debug"),
    verbosity_level = "verbose")
#> ℹ testing

msg("testing",
    levels_to_write = c("verbose", "debug"),
    verbosity_level = "quiet")
```

Note you are able to control what function to use when writing messages
to the console through the the argument `msg_fun` (default being
`msg_fun = cli::cli_alert_info`), and there are wrapper functions
`msg_debug` and `msg_success` available (see the documentation in using
`?msg`) which as default have `msg_fun = cli::cli_inform` and
`msg_fun = cli::cli_alert_success`, respectively.

### Controlling the verbosity level through options

The verbosity level can be specified as an argument withing the
function. However, this behavior can be controlled through package level
options. By default, `verbosity_level = NULL`, which means it will fetch
a `verbosity_level` option set in the `zephyr` package when the function
is used ‘directly’, and if the function is used inside another function,
it will fetch the `verbosity_level` option set in the package of the
function that called the `msg` function.

#### Setting a package option using the `options` package

Note that much more information about the `options` package is available
in the package’s [pkgdown](https://dgkf.github.io/options/), and here is
only provided a the minimal introduction to understand the usage in
context of the zephyr package.

In the `zephyr` package, we have set a package level `verbosity_level`
option by including the following code in a file below /R.

``` r
options::define_option(
  option = "verbosity_level",
  default = "verbose",
  desc = "Controls verbosity level in this package (overwritable using option
  `zephyr.verbosity_level` across all packages using `zephyr` functions).
  Options are 'debug', 'verbose' and 'quiet'",
  envir = getNamespace("zephyr")
)
#> 
#> verbosity_level = "verbose"
#> 
#>   Controls verbosity level in this package (overwritable using option
#>   `zephyr.verbosity_level` across all packages using `zephyr`
#>   functions).  Options are 'debug', 'verbose' and 'quiet'
#> 
#>   option  : zephyr.verbosity_level
#>   envvar  : R_ZEPHYR_VERBOSITY_LEVEL (evaluated if possible, raw string otherwise)
#>  *default : "verbose"
```

When the `verbosity_level` argument is not specified (i.e. left as
`verbosity_level = NULL`), the `msg` function will fetch the
`verbosity_level` option set in the `zephyr` package:

``` r
# Will not write a message
withr::with_envvar(list(R_ZEPHYR_VERBOSITY_LEVEL = "quiet"), {
  msg("testing")
})
```

``` r
# Will write a message
withr::with_options(list(zephyr.verbosity_level = "debug"), {
  msg_debug("testing")
})
#> testing
```

``` r
# Default set option is "verbose", so this will also write a message
msg_success("testing")
#> ✔ testing
```

#### Usage in R package development

The `verbosity_level` option can also be specified in a developer’s R
package.

When doing so, the behavior of the `msg` functions will be controlled
through that package level option, and users can then easily control the
verbosity level in the entire package. By default the `msg` functions
will fetch the `verbosity_level` option set in the package of the
function wherein the `msg` function is called.

##### Simulating creation of a package

We create an environment with an option where the `verbosity_level` have
been specified (**Note: if looking into the helper script that the
option is set differently than described above for a package - when you
define an option in your package, use the above approach**), and a
function `foo` that uses the `msg` function:

``` r
source("R/test_vignette_helpers.R")
foo_pkg <- create_env_with_fun(
  message = "Hello from foo_pkg!",
  fun_name = "foo",
  fun = function() {
    msg_debug("Inform my user the function is trying to do stuff")
    # Do stuff
    msg_success("Inform my user that stuff succeeded")
  }
)

# foo function
foo_pkg$foo
#> function () 
#> {
#>     msg_debug("Inform my user the function is trying to do stuff")
#>     msg_success("Inform my user that stuff succeeded")
#> }
#> <environment: 0x2f972098>

# Option set in package:
foo_pkg$.options
#> 
#> verbosity_level = NULL
#> 
#>   Option for testing
#> 
#>   option  : foo_pkg.verbosity_level
#>   envvar  : R_FOO_PKG_VERBOSITY_LEVEL (evaluated if possible, raw string otherwise)
#>  *default : default
```

###### Default (implicit) behavior when using `msg` functions in your package

As a default the `msg` functions will fetch the `verbosity_level` option
set in `foo_pkg` when calling `foo`:

``` r
# Does not write a message
withr::with_envvar(list(R_FOO_PKG_VERBOSITY_LEVEL = "quiet"), {
  foo_pkg$foo()
})

# Writes a message
withr::with_options(list(foo_pkg.verbosity_level = "debug"), {
  foo_pkg$foo()
})
#> Inform my user the function is trying to do stuff
#> ✔ Inform my user that stuff succeeded
```

However, a feature of the package (specifically the
`get_verbosity_level` function) is that you can set the verbosity level
for `zephyr` functionalities globally by setting the
`zephyr.verbosity_level` option, which will override individual package
level options:

``` r
# Writes a message
withr::with_options(list(
  foo_pkg.verbosity_level = "quiet",
  zephyr.verbosity_level = "verbose"
),
{
  foo_pkg$foo()
})
#> ✔ Inform my user that stuff succeeded
```

Setting an environmental variable of `R_ZEPHYR_VERBOSITY_LEVEL` will
only override the package level option in case the package level option
is not set using `foo_pkg_verbosity_level`:

``` r
# Will not write a message since the Zephyr environment variable overrides the package level
withr::with_envvar(list(
  R_ZEPHYR_VERBOSITY_LEVEL = "quiet",
  R_FOO_PKG_VERBOSITY_LEVEL = "verbose"
),
{
  foo_pkg$foo()
})

# Will write a message since option overrides the Zephyr environment variable
withr::with_envvar(list(R_ZEPHYR_VERBOSITY_LEVEL = "quiet"), {
  withr::with_options(list(foo_pkg.verbosity_level = "verbose"), {
    foo_pkg$foo()
  })
})
#> ✔ Inform my user that stuff succeeded
```

###### Controlling verbosity level through options with more transparency for the user

The default behavior described can be used to control verbosity level
through your package level options without having to specify anything.
In this case, write in the documentation of your functions that
verbosity level can be controlled through options.

In case a more transparent solution is wanted, a package level option
can be set, and then the
`zephyr::get_verbosity_level(env = getNamespace("foo_pkg"))` can be used
to set the default value in your function (or the
`options::opt("verbosity_level", env = getNamespace("foo_pkg"))`
function in case it’s not wanted to be able to override options “on a
global zephyr level”).

When creating a function `foo` in a package `foo_pkg` such a solution
would look like this:

``` r
foo <- function(my_arg,
                verbosity_level = zephyr::get_verbosity_level(env = getNamespace("foo_pkg"))) {
  zephyr::msg_debug("Inform my user the function is trying to do stuff",
                    verbosity_level = verbosity_level)
  # Do stuff
  zephyr::msg_success("Inform my user that stuff succeeded", verbosity_level = verbosity_level)
}
```

### Summary of how to use `zephyr` in your package

1.  Set a `verbosity_level` package level option in your package (see
    the `R/package_options.R` file in the `zephyr` package for an
    example as shown [in this earlier
    section](#setting-a-package-option-using-the-options-package))
2.  Develop your functions to include `zephyr` functionalities when you
    want to inform your user - allowing them to specify a
    `verbosity_level` directly as an argument or only through options.
3.  Write in the documentation of your functions that verbosity level
    can be controlled through options - see more about how to easily
    write reusable documentation from the `options` package
    [here](https://dgkf.github.io/options/articles/options.html#documentation)
