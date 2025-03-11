#' Consistently retrieve the name of an environment
#' @noRd
envname <- function(.envir) {
  if (is.function(.envir)) {
    .envir <- environment(.envir)
  }
  if (is.environment(.envir)) {
    if (nchar(environmentName(.envir)) > 0) {
      .envir <- environmentName(.envir)
    } else {
      .envir <- environmentName(parent.env(.envir))
    }
  }
  if (!is.character(.envir) || .envir == "") {
    return(NULL)
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
fix_env_class <- function(x, default = x) {
  if (is.null(x) || is.null(default) ||
        isTRUE(all.equal(class(x), class(default)))) {
    return(x)
  } else if (is.character(x) && !is.vector(default)) {
    return(eval(parse(text = x)))
  }

  if (is.character(x) && is.logical(default)) {
    x <- toupper(x)
    x[x %in% c("Y", "YES", "T")] <- "TRUE"
    x[x %in% c("N", "NO", "F")] <- "FALSE"
  }
  do.call(what = paste0("as.", class(default)[[1]]), args = list(x))
}

#' Return first non missing input
#' @noRd
coalesce_dots <- function(...) {
  dots <- rlang::list2(...)
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    if (!is.null(dot) && (!is.vector(dot) || !all(is.na(dot)))) {
      return(dot)
    }
  }
  return(NULL)
}
