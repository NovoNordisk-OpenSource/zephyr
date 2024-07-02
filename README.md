
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

## Zephyr message functionalities

These functionalities are intented to be used by developers of R
packages. First, we explain the basic principle of the functions and
then show how to use these in the context of R package development. Go
down to this section to see how to use it in your package.

### Basic features

There are functions `msg`, `msg_debug` and `msg_success` collectively
referred to in the remainder of this README as `msg` functions, and they
have a common documentation page that can be accessed `?msg`. These
write messages to the console dependent on a `verbosity_level` set.
Specifically, below it’s visible that the message is only written to the
console when the `verbosity_level` is matching a level in
`levels_to_write`.

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
to the console through the `msg_fun` argument, and there are wrapper
functions `msg_debug` and `msg_success` available (see the documentation
in using `?msg`).

### Controlling the verbosity level through options

In the above, you can see that you are able to specify the verbosity
level as an argument to the function. However, this behavior can be
controlled through package level options. By default,
`verbosity_level = NULL`, which means it will fetch a `verbosity_level`
option set in the `zephyr` package when the function is used ‘directly’,
and if the function is used inside another function, it will fetch the
`verbosity_level` option set in the package of the function that called
the `msg` function.

#### Setting a package option using the `options` package

Note that much more information about the `options` package is available
in the package’s [pkgdown](https://dgkf.github.io/options/), but here we
give the minimal introduction needed to understand the usage in context
of the zephyr package.

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

Not specifying a `verbosity_level` argument in the `msg` function will
fetch the `verbosity_level` option set in the `zephyr` package:

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
# Default set option is "verbose", so this Will also write a message
msg_success("testing")
#> ✔ testing
```

#### Usage in R package development

In the above, we describe how to set a `verbosity_level` option in a
package, and this same procedure can be performed in a developer’s R
package to set a package level option.

Then, the behavior of the `msg` functions will be controlled through
that package level option that users can then use to easily control the
verbosity level in the entire package. This is done by the fact that as
a default, the `msg` functions will fetch the `verbosity_level` option
set in the package of the function that called the `msg` function.

##### Simulating creation of a package

We create an environment with an option of `verbosity_level` set (**Note
if looking into the helper script that the option is set differently
than described above for a package - when you define an option in your
package, use the above approach**), and a function `foo` that uses the
`msg` function inside like so:

``` r
source("R/test_vignette_helpers.R")
foo_pkg <- create_env_with_fun(message = "Hello from foo_pkg!",
                               fun_name = "foo",
                               fun = function() {
                                 msg_debug("Inform my user the function is trying to do stuff")
                                 # Do stuff
                                 msg_success("Inform my user that stuff succeeded")
                               }
)

# foo function
foo_pkg$foo
#> function() {
#>                                  msg_debug("Inform my user the function is trying to do stuff")
#>                                  # Do stuff
#>                                  msg_success("Inform my user that stuff succeeded")
#>                                }
#> <environment: 0x4dfdc98>

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
withr::with_options(list(foo_pkg.verbosity_level = "quiet",
                         zephyr.verbosity_level = "verbose"), {
  foo_pkg$foo()
})
#> ✔ Inform my user that stuff succeeded
```

Setting an environmental variable of `R_ZEPHYR_VERBOSITY_LEVEL` will
only override the package level option in case the package level option
is not set using `foo_pkg_verbosity_level`:

``` r
# Will not write a message since the Zephyr environment variable overrides the package level
withr::with_envvar(list(R_ZEPHYR_VERBOSITY_LEVEL = "quiet",
                        R_FOO_PKG_VERBOSITY_LEVEL = "verbose"), {
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

Such a solution would look like creating a function `foo` in a package
`foo_pkg` like so:

``` r
foo <- function(my_arg,
                verbosity_level = zephyr::get_verbosity_level(env = getNamespace("foo_pkg"))) {
  zephyr::msg_debug("Inform my user the function is trying to do stuff",
              verbosity_level = verbosity_level)
  # Do stuff
  zephyr::msg_success("Inform my user that stuff succeeded",
              verbosity_level = verbosity_level)
}
```

### Summary of how to use `zephyr` in your package

1.  Set a `verbosity_level` package level option in your package (see
    the `R/package_options.R` file in the `zephyr` package for an
    example as shown [in this earlier section](#sec:set_opt))
2.  Develop your functions to include `zephyr` functionalities when you
    want to inform your user - allowing them to specify a
    `verbosity_level` directly as an argument or only through options.
3.  Write in the documentation of your functions that verbosity level
    can be controlled through options - see more about how to easily
    write resuable documentation from the `options` package
    [here](https://dgkf.github.io/options/articles/options.html#documentation)
