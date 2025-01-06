#' Consistently retrieve the name of an environment
#' @noRd
envname <- function(.envir) {
  if (is.function(.envir)) {
    .envir <- environment(.envir)
  }
  if (is.environment(.envir)) {
    .envir <- environmentName(.envir)
  }
  if (!is.character(.envir) || .envir == "") {
    cli::cli_abort("{.var .envir} must be of class {.cls function}, {.cls environment}, or {.cls character}")
  }
  return(.envir)
}

#' Get environment variable similar to if given as options
#' Returns NULL if unset, and splits on ";" if multiple values
#' @noRd
sys_getenv <- function(x) {
  x <- Sys.getenv(x)
  if (x == "") {
    return(NULL)
  }
  strsplit(x = x, split = ";") |>
    unlist()
}

#' Convert env to the required class
#' Special handling of logical vectors to also convert "truthy" values correct
#' @noRd
fix_env_class <- function(x, to_class) {
  if (is.null(x) || class(x) == to_class) {
    return(x)
  }
  if (is.character(x) && to_class == "logical") {
    x <- toupper(x)
    x[x %in% c("Y", "YES", "T")] <- "TRUE"
    x[x %in% c("N", "NO", "F")] <- "FALSE"
  }
  do.call(what = paste0("as.", to_class), args = list(x))
}

#' Return first non missing input
#' @noRd
coalesce_dots <- function(...) {
  dots <- rlang::list2(...)
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    if (!is.null(dot) && !all(is.na(dot))) {
      return(dot)
    }
  }
  return(NULL)
}
