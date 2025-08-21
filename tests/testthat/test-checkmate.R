test_that("report assertions", {
  skip_if_not_installed("checkmate")

  add_numbers <- function(a, b) {
    collection <- checkmate::makeAssertCollection()
    checkmate::assert_numeric(x = a, add = collection)
    checkmate::assert_numeric(x = b, add = collection)
    report_checkmate_assertions(collection)
    return(a + b)
  }

  add_numbers(1, 2) |>
    expect_equal(3)

  err <- add_numbers(1, "b") |>
    expect_error()

  err$call |>
    expect_equal(str2lang("add_numbers(1,\"b\")"))

  err$message |>
    as.character() |>
    expect_equal("Invalid input(s):")

  err$body |>
    as.character() |>
    expect_equal(c("Variable 'b': Must be of type 'numeric', not 'character'."))
})
