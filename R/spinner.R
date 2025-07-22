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

    msg <- mq$receive(timeout_ms = 250)
    if (!is.null(msg) && msg == "STOP") {
      cat("\r", strrep(" ", nchar(text) + 10), "\r", sep = "")
      utils::flush.console()
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
#' @param timeout Timeout in milliseconds
#' @return A list containing spinner context information
#' @keywords internal
#' @examplesIf FALSE
#' # Start a spinner, do some work, then stop it
#' ctx <- start_spinner("Starting some long task...")
#' Sys.sleep(1)
#' stop_spinner(ctx)
#'
#' # It is recommended to always pair with stop_spinner()
#' # to properly clean up resources.
#' @noRd
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
#' @keywords internal
#' @examplesIf FALSE
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
#' @noRd
stop_spinner <- function(ctx, status = NULL, error = FALSE) {
  if (is.null(ctx)) {
    return(invisible())
  }
  status_msg <- if (is.null(status)) {
    paste0("[OK] ", ctx$msg, " ")
  } else if (error) {
    paste0("[ERR] ", ctx$msg, ": ", status, " ")
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
#' @param msg Message to display alongside the spinner.
#' @keywords internal
#' @examplesIf FALSE
#' # Execute a function with a spinner
#' spinner(function() Sys.sleep(1), msg = "Waiting with spinner...")
#'
#' # Any return value is returned from the function you provided:
#' result <- spinner(function() {
#'   Sys.sleep(0.5)
#'   1 + 1
#' })  # Will automatically display "Running..."
#' print(result)
#' @noRd
spinner <- function(x = NULL, msg = NULL, formatted = FALSE) {
  if (!is.function(x)) {
    stop("Please pass a function to spinner()")
  }
  if (!is.null(msg)) {
    if (!formatted) {
      formatted_msg <- deparse(msg) |>
        paste(collapse = " ") |>
        (\(text) as.character(text))() |>
        (\(expr_text) cli::format_message("Running: {.code {expr_text}}"))()
    } else {
      formatted_msg <- msg
    }
    if (!interactive()) {
      non_interactive_msg <- paste0(
        formatted_msg,
        " (Session non-interactive -> spinner disabled)"
      )
      zephyr::msg_info(non_interactive_msg)
    } else {
      rlang::check_installed("callr")
      rlang::check_installed("interprocess")
      ctx <- start_spinner(formatted_msg)
    }
  }
  withr::defer(stop_spinner(ctx = if (exists("ctx")) ctx else NULL))
  return(x())
}
#' `spinner` wrapper to avoid LHS priority eval limitations with `|>`
#'
#' This function evaluates an expression while displaying a spinner animation
#' with a custom message.
#'
#' @param expr The expression to evaluate
#' @param msg The message to display alongside the spinner default is `'Running: {.expr}'`
#' @return The result of evaluating `expr`
#' @examples
#' #' # Simple delay with spinner
#' @examplesIf FALSE
#' with_spinner(Sys.sleep(2), "Waiting for 2 seconds")
#'
#' # Complex expressions
#' with_spinner({
#'   Sys.sleep(1)
#'   Sys.sleep(1)
#'   "Result"
#' }, "Processing complex operation")
#' @export
with_spinner <- function(expr, msg = "Running: {.expr}") {
  expr_quo <- rlang::enquo(expr)

  rlang::quo_get_expr(expr_quo) |>
    deparse() |>
    (\(str) {
      paste0(trimws(str[if (trimws(str[1]) == "{") 2 else 1]))
    })() |>
    (\(display) {
      tryCatch(
        glue::glue(msg, .expr = display, .open = "{", .close = "}"),
        error = \(e) paste0("Running: ", display)
      )
    })() |>
    (\(formatted_msg) {
      tryCatch(
        {
          # Capture any print output from the expression
          output_capture <- utils::capture.output(
            {
              result <- spinner(
                \() rlang::eval_tidy(expr_quo, env = rlang::caller_env()),
                msg = formatted_msg,
                formatted = TRUE
              )
            },
            type = "output"
          )
          has_output <- length(output_capture) > 0 && !all(output_capture == "")

          if (has_output) {
            for (line in output_capture) {
              if (line != "") {
                zephyr::msg_info(line)
              }
            }
          } else {
            zephyr::msg_success(formatted_msg)
          }

          result
        },
        warning = function(w) {
          zephyr::msg_warning(formatted_msg)
          warning(w)
          NULL
        },
        error = function(e) {
          zephyr::msg_danger(formatted_msg)
          stop(e)
        }
      )
    })()
}
