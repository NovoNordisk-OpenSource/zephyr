nested_env <- new.env()
nested_env_child <- new.env(parent = nested_env)

#' Return the nested environment for testing
#' @export
get_nested_env_child <- function() {
  nested_env_child
}