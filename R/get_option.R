get_option <- function(opt_name = "verbosity_level",
                       global_opt_name = paste0("atmos.", opt_name)) {

  # If a global option is set, use that
  global_opt <- getOption(global_opt_name)
  if (!is.null(global_opt)) {
    return(global_opt)
  }

  # Get the call of the "top function" and get the namespace of that function
  call_of_top_fun <- base::sys.calls()[[1]]
  call_name <- rlang::call_name(call_of_top_fun)
  ns_of_top_fun <- environment(get(call_name))

  # Get the option from the namespace of the package used "from start"
  return(options::opt(opt_name, env = ns_of_top_fun))
}
