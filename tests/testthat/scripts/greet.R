#' Greet someone
#' @description
#' Test function for easy integration testing of how to a zephyr_option
#'
#' @param name Who to greet
#' @inheritParams testpkg-options-params
#' @examples
#' greet("Bob")
#' @export
greet <- function(
    name,
    greeting = zephyr::get_option("greeting", .envir = "testpkg")) {

  zephyr::msg_info(paste(greeting, name))
  return(invisible(name))
}

zephyr::create_option(
  name = "greeting",
  default = "hello"
)
