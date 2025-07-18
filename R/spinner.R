#' Spinner process function (internal helper)
#'
#' @param id Spinner (IPC) ID
#' @param text Spinner message
#' @keywords internal
#' @noRd
.spinner_worker <- function(id, text) {
  mq <- interprocess::msg_queue(name = id, assert = "exists")
  sem <- interprocess::semaphore(name = paste0("s", id), assert = "exists")
  spinner_chars <- c("-", "\\", "|", "/")
  idx <- 1
  sem$post()
  repeat {
    cat("\r", spinner_chars[idx], " ", text, " ", sep = "")
    utils::flush.console()
    idx <- (idx %% 4) + 1
    if (
      !is.null(mq$receive(timeout_ms = 100)) &&
        (mq$receive(timeout_ms = 0) == "STOP")
    ) {
      cat("\r", strrep(" ", nchar(text) + 10), "\r", sep = "")
      status <- mq$receive(timeout_ms = 100)
      cat(
        if (!is.null(status)) {
          paste0(status, " ", text)
        } else {
          paste0("[OK] ", text, " ")
        },
        "\n"
      )
      sem$post()
      break
    }
  }
}

#' Start a CLI spinner
#'
#' Start a spinner process to indicate work in progress on the CLI.
#'
#' @param msg Message to display alongside spinner
#' @return A list containing spinner context information
#' @examples
#' \dontrun{
#' # Start a spinner, do some work, then stop it
#' ctx <- start_spinner("Starting some long task...")
#' Sys.sleep(1)
#' stop_spinner(ctx)
#'
#' # It is recommended to always pair with stop_spinner()
#' # to properly clean up resources.
#' }
#' @export
start_spinner <- function(msg = "Processing... ") {
  spinner_id <- interprocess::uid()
  mq <- interprocess::msg_queue(
    name = spinner_id,
    max_count = 10,
    max_nchar = 1024,
    cleanup = TRUE
  )
  sem <- interprocess::semaphore(
    name = paste0("s", spinner_id),
    value = 0,
    cleanup = TRUE
  )
  spinner_process <- callr::r_bg(
    .spinner_worker,
    args = list(id = spinner_id, text = msg),
    supervise = TRUE,
    stdout = "",
    stderr = ""
  )

  if (!sem$wait(timeout_ms = 1000)) {
    warning("Spinner process didn't start properly. ")
    if (spinner_process$is_alive()) {
      spinner_process$kill()
    }
    mq$remove()
    sem$remove()
    return(NULL)
  }

  list(
    spinner_id = spinner_id,
    mq = mq,
    sem = sem,
    process = spinner_process,
    msg = msg
  )
}

#' Stop a CLI spinner
#'
#' Stop an active CLI spinner and display a status message.
#'
#' @param ctx Spinner context returned by \code{start_spinner}.
#' @param status Status message to display (NULL for default success message).
#' @param error Logical; whether this is an error status.
#' @return Invisibly returns NULL.
#' @examples
#' \dontrun{
#' # Start a spinner, do work, then stop the spinner with default success
#' ctx <- start_spinner("Doing work with spinner...")
#' Sys.sleep(1)
#' stop_spinner(ctx)
#'
#' # Start a spinner and stop with a custom status
#' ctx <- start_spinner("Custom status example...")
#' Sys.sleep(1)
#' stop_spinner(ctx, status = "[OK] Custom complete!")
#'
#' # Start a spinner and stop with an error message
#' ctx <- start_spinner("Failing operation...")
#' Sys.sleep(1)
#' stop_spinner(ctx, status = "Oops, something went wrong!", error = TRUE)
#' }
#' @export
stop_spinner <- function(ctx, status = NULL, error = FALSE) {
  if (is.null(ctx)) {
    return(invisible())
  }
  status_msg <- if (is.null(status)) {
    paste0("[OK] ", ctx$msg, " ")
  } else if (error) {
    paste0("[ERR] ", ctx$msg, ": ", status, " ")
  } else {
    status
  }
  ctx$mq$send("STOP")
  ctx$mq$send(status_msg)
  ctx$sem$wait(timeout_ms = 500)
  if (ctx$process$is_alive()) {
    ctx$process$kill()
  }
  ctx$mq$remove()
  ctx$sem$remove()
  invisible()
}

#' Display a CLI spinner while running a function
#'
#' Runs a function while displaying a CLI spinner animation.
#' The spinner appears while the given function is executing, and
#' disappears when it finishes (whether successfully or not).
#'
#' @param x A function to execute while the spinner is running.
#' @param msg Message to display alongside the spinner.#'
#' @examples
#' \dontrun{
#' # Execute a function with a spinner
#' spinner(function() Sys.sleep(1), msg = "Waiting with spinner...")
#'
#' # Any return value is returned from the function you provided:
#' result <- spinner(function() {
#'   Sys.sleep(0.5)
#'   1 + 1
#' }, msg = "Calculating 1+1...")
#' print(result)
#' }
#' @export
spinner <- function(x = NULL, msg = "Processing... ") {
  if (!is.function(x)) {
    stop("Please pass a function to spinner()")
  }
  if (
    !requireNamespace("interprocess", quietly = TRUE) ||
      !requireNamespace("callr", quietly = TRUE)
  ) {
    return(x())
  }
  ctx <- start_spinner(msg)
  on.exit(stop_spinner(ctx))
  x()
}
spinner <- function(x = NULL, msg = "Processing... ") {
  if (!is.function(x)) {
    stop("Please pass a function to spinner()")
  }
  ctx <- start_spinner(msg)
  on.exit(stop_spinner(ctx))
  x()
}
#' `spinner` wrapper to avoid LHS priority eval limitations with `|>`
#'
#' This function evaluates an expression while displaying a spinner animation
#' with a custom message.
#'
#' @param expr The expression to evaluate
#' @param msg The message to display alongside the spinner
#' @return The result of evaluating `expr`
#' @examples
#' #' # Simple delay with spinner
#' \dontrun{
#' with_spinner(Sys.sleep(2), "Waiting for 2 seconds")
#'
#' # Complex expressions
#' with_spinner({
#'   Sys.sleep(1)
#'   Sys.sleep(1)
#'   "Result"
#' }, "Processing complex operation")
#' }
#'@export
with_spinner <- function(expr, msg = "Processing... ") {
  return(spinner(
    function() {
      rlang::eval_tidy(rlang::enquo(expr), env = rlang::caller_env())
    },
    msg = msg
  ))
}
