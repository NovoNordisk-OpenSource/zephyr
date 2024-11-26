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
