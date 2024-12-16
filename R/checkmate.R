#' Report collection of assertions
#' @description
#' Improved reporting of `AssertCollections` created with the [checkmate::makeAssertCollection()]
#' using [cli::cli_abort()] instead of [checkmate::reportAssertions()] in order to provide a more
#' informative error message.
#'
#' The function is intended to be used inside a function that performs assertions on its input arguments.
#' See below for an example.
#' @param collection A collection of assertions created with [checkmate::makeAssertCollection()]
#' @param msg [character()] Header of the error message if any assertions failed
#' @param env [environment()] Environment to use for the error message
#' @examples
#' add_numbers <- function(a, b) {
#'   collection <- checkmate::makeAssertCollection()
#'   checkmate::assert_numeric(x = a, add = collection)
#'   checkmate::assert_numeric(x = b, add = collection)
#'   report_checkmate_assertions(collection)
#'   return(a + b)
#' }
#'
#' add_numbers(1, 2)
#' try(add_numbers(1, "b"))
#' try(add_numbers("a", "b"))
#'
#' @export

report_checkmate_assertions <- function(collection, msg = "Invalid input(s):", env = parent.frame()) {
  checkmate::assert_class(collection, "AssertCollection")

  if (!collection$isEmpty()) {
    c(msg, rlang::set_names(collection$getMessages(), "x")) |>
      cli::cli_abort(.envir = env)
  }
  invisible(TRUE)
}
