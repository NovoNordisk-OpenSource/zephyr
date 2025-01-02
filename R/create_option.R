#' Create option
#' @export
create_option <- function(name, default, description) {

  envir <- parent.frame()

  spec <- structure(
    list(
      default = default,
      name = name,
      description = description,
      environment = envname(envir)
      ),
    class = c("zephyr_option")
  )

  if (!exists(".zephyr_options", envir = envir, inherits = FALSE)) {
    envir[[".zephyr_options"]] <- new.env(parent = emptyenv())
  }

  envir[[".zephyr_options"]][[name]] <- spec

  return(invisible(spec))
}

#' @export
format.zephyr_option <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h3(x$name)
    cli::cli_text(x$description)
    cli::cli_ul(
      c(
        "Default: {.code {deparse1(x$default)}}",
        "Option: {.code {tolower(x$environment)}.{x$name}}",
        "Environment: {.code R_{toupper(x$environment)}_{toupper(x$name)}}"
      )
    )
  })
}

#' @export
print.zephyr_option <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
