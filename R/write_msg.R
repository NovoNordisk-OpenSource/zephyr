#' Write messages based on verbosity level
#'
#' @description
#'
#' Write based on verbosity level set as option in package
#'
#' @param message `character` of message to write
#' @param levels_to_write `character` vector of levels of verbosity for which
#' to display the message
#' @param msg_fun `function` taking `message` as first argument. Usually a
#' `cli_...` function
#' @param opt_name `character` name of the option. Passed to [get_opt()]
#' @param global_opt_name `character` name of the global option which, if set,
#' will overwrite the package level option. Passed to [get_opt()]
#' @param ... Additional arguments passed to `msg_fun`
#'
#'
#' @examples
#' # Use the `write_msg` function to fx. create headlines for big steps in the
#' # TFL creation process
#' write_msg("Initialising {.field ingest_tfl} object",
#'           levels_to_write = c("verbose", "debug"),
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
                      levels_to_write = c("verbose", "debug"),
                      msg_fun = cli::cli_h1,
                      opt_name = "verbosity_level",
                      global_opt_name = paste0("atmos.", opt_name),
                      ...) { # TODO add a destination argument to control
  # whether to write to console, write to a file, etc.

  match.arg(levels_to_write,
            choices = c("quiet", "verbose", "debug"),
            several.ok = TRUE)

  # Get value of option
  verbosity_level <- get_opt(opt_name = opt_name,
                             global_opt_name = global_opt_name)

  if (verbosity_level %in% levels_to_write) {
    msg_fun(message, ...)
  }

  invisible()
}
