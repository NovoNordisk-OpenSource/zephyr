#' Get Package Option
#'
#' This function searches for a specified option within a package environment.
#' It looks for objects that either end with the given option name or match it exactly.
#' The search is case-insensitive.
#'
#' @param option_name A character string specifying the name of the option to search for.
#' @param pkg_env The package environment to search in. This should be an environment object.
#'
#' @return The name of the matching option if found, otherwise NULL.
#'
#' @details
#' The function first searches in the main package environment. If the option is not found there,
#' it checks in the `.options` environment if it exists within the package environment.
#' If multiple matching options are found, it returns the first one and issues a warning.
#'
#' @note
#' This function does not return the value of the option, only its name.
#' To get the value, use `opt_pkg(result, envir = pkg_env)` on the returned result.
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_package' is loaded and has an option 'verbosity_level'
#' pkg_env <- asNamespace("my_package")
#' option_name <- get_package_option("verbosity_level", pkg_env)
#' if (!is.null(option_name)) {
#'   option_value <- opt_pkg(option_name, envir = pkg_env)
#'   print(paste("Value of", option_name, "is", option_value))
#' }
#' }
#'
#' @export
get_package_option <- function(option_name, pkg_env) {
  # Function to check in an environment for the option
  check_env <- function(env, opt_name) {
    all_objects <- ls(env)
    if (!is.null(all_objects)) {all_objects <- ls(env$.options)}
    # Match objects that either match option_name exactly or end with .option_name
    pattern <- paste0("^", opt_name, "$|\\.", opt_name, "$")
    matching_objects <- grep(pattern, all_objects, ignore.case = TRUE, value = TRUE)

    if (length(matching_objects) > 0) {
      if (length(matching_objects) > 1) {
        warning(paste("Multiple options found for", opt_name, ". Using the first one."))
        matching_objects <-  matching_objects[1]
      }
      return(matching_objects)
    }
    return(NULL)
  }


  # Check in the main environment
  result <- check_env(pkg_env, option_name)
  if (!is.null(result)) return(result)

  # If not found, check in the .options environment if it exists
  if (exists(".options", envir = pkg_env) && is.environment(pkg_env$.options)) {
    result <- check_env(pkg_env$.options, option_name)
    if (!is.null(result)) return(result)
  }

  warning(paste("No option found for", option_name, "in the package environment."))
  return(NULL)
}
