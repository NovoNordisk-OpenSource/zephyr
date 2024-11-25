# create_env_with_fun <- function(fun_name = "foo",
#                                 message = "test",
#                                 add_option = TRUE,
#                                 default = "verbose",
#                                 fun = function(msg_fun = msg, ...) {
#                                   Sys.sleep(0.5)
#                                   msg_fun(message, ...)
#                                 }) {
#   e <- rlang::env()
#   environment(fun) <- e
#   assign(fun_name, fun, envir = e)
#
#   if (add_option) {
#     # Using option spec followed by unexported set_option_spec instead of using
#     # define_option due to a bit weird behavior with environments during testing
#     option <- options::option_spec("verbosity_level",
#                                    default = default,
#                                    desc = "Option for testing",
#                                    option_name = paste0(fun_name, "_pkg.verbosity_level"),
#                                    envvar_name = paste0("R_", toupper(fun_name), "_PKG_VERBOSITY_LEVEL"))
#     set_option_spec <- utils::getFromNamespace("set_option_spec", "options")
#     set_option_spec("verbosity_level", option, env = e)
#   }
#
#   return(e)
# }
