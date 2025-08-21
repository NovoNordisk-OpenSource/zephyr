#' Report collection of assertions
#' @description
#' Improved reporting of an `AssertCollection` created with the
#' [checkmate::makeAssertCollection()] using [cli::cli_abort()] instead of
#' [checkmate::reportAssertions()] in order to provide a more
#' informative error message.
#'
#' The function is intended to be used inside a function that performs
#' assertions on its input arguments.
#' @param collection `[AssertCollection]` A collection of assertions created
#' with [checkmate::makeAssertCollection()].
#' @param message `[character(1)]` string with the header of the error message
#' if any assertions failed
#' @param .envir The `[environment]` to use for the error message.
#' Default `parent.frame()` will be sufficient for most use cases.
#' @returns `invisible(TRUE)`
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

report_checkmate_assertions <- function(collection,
                                        message = "Invalid input(s):",
                                        .envir = parent.frame()) {
  rlang::check_installed("checkmate")

  checkmate::assert_class(collection, "AssertCollection")

  if (!collection$isEmpty()) {
    c(message, rlang::set_names(collection$getMessages(), "x")) |>
      cli::cli_abort(.envir = .envir)
  }
  invisible(TRUE)
}
