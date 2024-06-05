#' Report collection of assertations
#' @description
#' Improved reporting of `AssertColellections` created with the [checkmate::makeAssertCollection()] package
#' using [cli::cli_abort()] instead of [checkmate::reportAssertions()] in order to provide a more
#' informative error message.
#'
#' THe function is intended to be used inside a function that performs assertations on its input arguments.
#' See below for an example.
#' @param collection A collection of assertations created with [checkmate::makeAssertCollection()]
#' @param msg [character()] Header of the error message if any assertations failed
#' @param env [environment()] Environment to use for the error message
#' @examples
#' add_numbers <- function(a, b) {
#'   collection <- checkmate::makeAssertCollection()
#'   checkmate::assert_numeric(x = a, add = collection)
#'   checkmate::assert_numeric(x = b, add = collection)
#'   report_checkmate_assertations(collection)
#'   return(a + b)
#' }
#' \donttest{
#' add_numbers(1, 2)
#' add_numbers(1, "b")
#' add_numbers("a", "b")
#' }
#' @export

report_checkmate_assertations <- function(collection, msg = "Invalid input(s):", env = parent.frame()) {
  checkmate::assert_class(collection, "AssertCollection")

  if (!collection$isEmpty()) {
    c(msg, rlang::set_names(collection$getMessages(), "x")) |>
      cli::cli_abort(.envir = env)
  }
  invisible(TRUE)
}
