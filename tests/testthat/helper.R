# Helper function to reset all options and environment variables
reset_all <-  function() {
  options(list(
    "testpkg.verbosity_level" = NULL,
    "zephyr.verbosity_level" = NULL
  ))
  Sys.unsetenv("R_TESTPKG_VERBOSITY_LEVEL")
  Sys.unsetenv("R_ZEPHYR_VERBOSITY_LEVEL")

  # Clear package-specific options more safely
  env <- parent.frame()
  if (exists(".options", envir = env, inherits = FALSE)) {
    if (is.environment(env$.options)) {
      rm(list = ls(envir = env$.options), envir = env$.options)
    }
  }
  remove_option_pkg("verbosity_level", envir = getNamespace("zephyr"))
}

# Helper function to simulate package environment
simulate_package_env <-  function(parent_pkg_name, new_pkg_name) {
  parent_env <- as.environment(paste0("package:", parent_pkg_name))
  pkg_env <- new.env(parent = parent_env)

  # Set the name of the new environment using attr
  attr(pkg_env, "name") <- paste0("package:", new_pkg_name)

  r_files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
  for (file in r_files) {
    sys.source(file, envir = pkg_env)
  }
  pkg_env
}

create_env_with_fun <- function(fun_name = "foo",
  message = "test",
  add_option = TRUE,
  default = "verbose",
  fun = function(msg_fun = msg, ...) {
    Sys.sleep(0.5)
    msg_fun(message, ...)
  }) {
  e <- rlang::env()
  environment(fun) <- e
  assign(fun_name, fun, envir = e)

  if (add_option) {
    # Use define_option_pkg from zephyr
    define_option_pkg("verbosity_level",
      default = default,
      desc = "Option for testing",
      option_name = paste0(fun_name, "_pkg.verbosity_level"),
      envvar_name = paste0("R_", toupper(fun_name), "_PKG_VERBOSITY_LEVEL"),
      envir = e)
  }

  return(e)
}
