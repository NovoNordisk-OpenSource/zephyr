#' Start a CLI spinner
#'
#' @param msg Message to display alongside spinner
#' @return A list containing spinner context information
#' @keywords internal
#' @noRd
start_spinner <- function(msg = "Processing...") {
  # Create resources with unique ID
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

  # Start background spinner process
  spinner_process <- callr::r_bg(
    function(id, text) {
      requireNamespace(interprocess)

      mq <- interprocess::msg_queue(name = id, assert = "exists")
      sem <- interprocess::semaphore(name = paste0("s", id), assert = "exists")

      spinner_chars <- c("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")
      idx <- 1
      sem$post()

      while (TRUE) {
        cat("\r", text, " ", spinner_chars[idx], " ", sep = "")
        utils::flush.console()
        idx <- (idx %% length(spinner_chars)) + 1

        message <- mq$receive(timeout_ms = 100)
        if (!is.null(message) && message == "STOP") {
          cat("\r", strrep(" ", nchar(text) + 10), "\r", sep = "")

          status_message <- mq$receive(timeout_ms = 100)
          cat(
            if (!is.null(status_message)) {
              status_message
            } else {
              paste0(text, " ✓")
            },
            "\n"
          )

          sem$post()
          break
        }
      }
    },
    args = list(id = spinner_id, text = msg),
    supervise = TRUE,
    stdout = "",
    stderr = ""
  )

  # Check if spinner started successfully
  if (!sem$wait(timeout_ms = 1000)) {
    warning("Spinner process didn't start properly.")
    if (spinner_process$is_alive()) {
      spinner_process$kill()
    }
    mq$remove()
    sem$remove()
    return(NULL)
  }

  # Return spinner context
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
#' @param spinner_ctx Spinner context returned by start_spinner
#' @param status Status message to display (NULL for default success message)
#' @param error Whether this is an error status
#' @return Nothing
#' @keywords internal
#' @noRd
stop_spinner <- function(ctx, status = NULL, error = FALSE) {
  if (is.null(ctx)) {
    return(invisible())
  }

  status_msg <- if (is.null(status)) {
    paste0(ctx$msg, " ✓")
  } else if (error) {
    paste0(ctx$msg, " Error: ", status)
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
#' @param x function to execute in the foreground
#' @param msg Message to display alongside spinner
#' @keywords internal
#' @noRd
spinner <- function(x = NULL, msg = "Processing...") {
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
#'
#' @examples
#' # Simple delay with spinner
#' with_spinner(Sys.sleep(2), "Waiting for 2 seconds")
#'
#' # Complex expressions
#' with_spinner({
#'   Sys.sleep(1)
#'   Sys.sleep(1)
#'   "Result"
#' }, "Processing complex operation")
#'
#' @noRd
with_spinner <- function(expr, msg = "Processing...") {
  return(spinner(
    function() {
      rlang::eval_tidy(rlang::enquo(expr), env = rlang::caller_env())
    },
    msg = msg
  ))
}
