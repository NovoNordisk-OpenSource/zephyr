#' Write messages based on verbosity level
#'
#' @description
#'
#' Write based on verbosity level set as option in package
#'
#' @param message `character` of message to write
#' @param x Code to be printed based on verbosity level
#' @param write_for_levels `character` vector of levels of verbosity for which
#' to display the message
#' @param msg_fun `function` taking `message` as first argument. Usually a
#' `cli_...` function
#' @param ... Additional arguments passed to `msg_fun`
#' @param write_for_levels `character` vector of levels of verbosity for which
#'
#' @examples
#' # Use the `write_msg` function to fx. create headlines for big steps in the
#' # TFL creation process
#' write_msg("Initialising {.field ingest_tfl} object",
#'           write_for_levels = c("verbose", "debug"),
#'           msg_fun = cli::cli_h1)
#'
#' ingest_init(pharmaverseadam::adae, filter = SAFFL == "Y")
#'
#' # Use the `with_verbose` function to
#' data %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   with_verbose()
#'
#' @export
write_msg <- function(message,
                      write_for_levels = c("verbose", "debug"),
                      msg_fun = cli::cli_h1,
                      ...) { # TODO add a destination argument to control
  # whether to write to console, write to a file, etc.

  match.arg(write_for_levels,
            choices = c("quiet", "verbose", "debug"),
            several.ok = TRUE)

  verbosity_level <- get_option("verbosity_level")

  if (verbosity_level %in% write_for_levels) {
    msg_fun(message, ...)
  }

  invisible()
}
