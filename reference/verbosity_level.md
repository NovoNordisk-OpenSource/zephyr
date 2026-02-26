# Verbosity level to control package behavior

In zephyr we define a central verbosity level to control the amount of
messages the user receives when using zephyr and other packages in the
ecosystem.

Verbosity level can be any of the four values below:

1.  `quiet`: No messages are displayed.

2.  `minimal`: Only essential messages are displayed.

3.  `verbose` (*default*): More informative messages are displayed.

4.  `debug`: Detailed messages for debugging are displayed.

See
[`use_zephyr()`](https://novonordisk-opensource.github.io/zephyr/reference/use_zephyr.md)
and
[msg](https://novonordisk-opensource.github.io/zephyr/reference/msg.md)
for how to implement the use of verbosity levels in your package and its
functions.

Verbosity level is a special kind of option that can be scoped both for
a specific package and globally for the ecosystem (assigned to the
zephyr package). It can be set using either R
[`options()`](https://rdrr.io/r/base/options.html) or environment
variables.

Verbosity level is retrieved using the
[`get_verbosity_level()`](https://novonordisk-opensource.github.io/zephyr/reference/get_verbosity_level.md)
function. Since the level can have multiple scopes, the following
hierarchy is used:

1.  Package specific option: `{pkgname}.verbosity_level`

2.  Package specific environment variable: `R_{PKGNAME}_VERBOSITY_LEVEL`

3.  Ecosystem wide option: `zephyr.verbosity_level`

4.  Ecosystem wide environment variable (`R_ZEPHYR_VERBOSITY_LEVEL`)

5.  Default value specified in zephyr (`verbose`, see above).

In order to see all registered verbosity levels across scopes call
[`get_all_verbosity_levels()`](https://novonordisk-opensource.github.io/zephyr/reference/get_all_verbosity_levels.md).

## Examples

``` r
get_verbosity_level("zephyr")
#> [1] "verbose"
get_all_verbosity_levels()
#>    zephyr 
#> "verbose" 
```
