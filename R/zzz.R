.onLoad <- function(libname, pkgname) {
  # Set default options when package is loaded
  define_option_pkg.zephyr(
    package = "zephyr",
    option = "verbosity_level",
    default = "verbose",
    desc = "Controls verbosity level in zephyr package.
    Options are 'debug', 'verbose', and 'quiet'"
  )
}
