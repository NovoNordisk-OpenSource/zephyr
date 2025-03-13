# zephyr 0.1.2

* Fixes a bug in `msg()` where the verbosity level of the package using zephyr
was not respected.

# zephyr 0.1.1

* Fixes a bug where `list_options()` was being able to document options with 
non vector default values, or with length different from one.
* `get_option()` now gives consistent return for non vector options, 
e.g. functions.

# zephyr 0.1.0

* Initial release to CRAN.
* Provides a structured framework for consistent user communication and 
configuration management for package developers.
