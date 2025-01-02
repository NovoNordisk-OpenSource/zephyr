#' Get option
#' @export
get_option <- function(name, .envir = sys.function(which = -1)) {
  if (!is.character(name) || length(name) > 1) {
    cli::cli_abort("{.var name} must be of class {.cls character} and length {.val 1}")
  }

  env <- envname(.envir)

  default <- getNamespace(env)[[".zephyr_options"]][[name]][["default"]]

  coalesce_dots(

    paste(env, name, sep = ".") |>
      tolower() |>
      getOption(),

    paste("R", env, name, sep = "_") |>
      toupper() |>
      sys_getenv() |>
      fix_env_class(to_class = class(default)),

    default
  )
}

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

#' @noRd
sys_getenv <- function(x) {
  x <- Sys.getenv(x)
  if (x == "") return(NULL)
  strsplit(x = x, split = ";|:") |>
    unlist()
}

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

#' @noRd
coalesce_dots <- function(...) {
  dots <- rlang::list2(...)
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    if (!is.null(dot) && !any(is.na(dot))) {
      return(dot)
    }
  }
  return(NULL)
}
