#' Get option using the `options` package, allowing a global option
#'
#' @description
#' Get option using the `options` package, as a default from the package from
#' which the function at the top of the call stack originates, so other
#' functions in this package can be used inside any package, and it will
#' automatically use the options set within that package. Also, the
#' function allows a global option to overwrite individual package options.
#'
#' @param opt_name `character` name of the option
#' @param global_opt_name `character` name of the global option which, if set,
#' will overwrite the package level option
#' @param env `environment` the environment to get the option from. As default
#' the namespace of the package where the function at the top of the call stack
#' comes from
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' callisto::hello <- function() {
#'   zephyr::write_msg("testing")
#' }
#'
get_opt <- function(opt_name = NULL,
                    global_opt_name = paste0("atmos.", opt_name),
                    env = ns_of_call()) {

  # If a global option is set, use that
  global_opt <- getOption(global_opt_name)
  if (!is.null(global_opt)) {
    return(global_opt)
  }

  # Get the option from the namespace of the package used "from start"
  return(options::opt(opt_name, env = env))
}

#' Get the namespace of function in the call stack
#'
#' @description Get the namespace of the function somewhere in the call stack.
#' Default behavior will get the namespace of the package from which the top
#' function in the stack comes from
#'
#' @param which Passed onto [base::sys.call()]
#'
#' @return The namespace of the package that function in `sys.call(which)`
#' belongs to
#' @export
#'
#' @examples
#' # Get the namespace of package from which the top function in the call stack
#' # comes from
#' ns_of_call()
ns_of_call <- function(which = 1) {
  call_of_top_fun <- base::sys.call(which = which)
  call_name <- rlang::call_name(call_of_top_fun)

  return(environment(get(call_name)))
}
