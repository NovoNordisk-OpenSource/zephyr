options::define_option(
  option = "verbosity_level",
  default = "verbose",
  desc = "Controls verbosity level in this package (overwritable using option
  `zephyr.verbosity_level` across all packages using `zephyr` functions).
  Options are 'debug', 'verbose' and 'quiet'",
  envir = getNamespace("zephyr")
)
