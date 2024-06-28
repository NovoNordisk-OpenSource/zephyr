create_env_with_fun <- function(fun_name = "foo",
                                message = "test",
                                default = "verbose",
                                fun = function(a = 1, b = 2, ...) {
                                  msg(message, ...)
                                  a+b
                                }) {
  e <- rlang::env()
  environment(fun) <- e
  assign(fun_name, fun, envir = e)

  # Using option spec followed by unexported set_option_spec instead of using
  # define_option due to a bit weird behavior with environments during testing
  option <- options::option_spec("verbosity_level",
                                 default,
                                 desc = "tst",
                                 option_name = paste0(fun_name, "_pkg.verbosity_level"),
                                 envvar_name = paste0("R_", toupper(fun_name), "_PKG_VERBOSITY_LEVEL"))
  options:::set_option_spec("verbosity_level", option, env = e)

  return(e)
}
