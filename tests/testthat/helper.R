# Helper function to simulate package environment
simulate_package_env <- function(new_pkg_name) {
  pkg_env <- new.env(parent = emptyenv())

  # Set the name of the new environment using attributes
  attr(pkg_env, "name") <- paste0(new_pkg_name)

  # # Create verbosity option
  create_option(
    name = "verbosity_level",
    default = NA_character_,
    description = "Dummy verbosity in this package",
    .envir = pkg_env
  )

  return(pkg_env)
}
