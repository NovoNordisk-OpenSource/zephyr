# nocov start
.onLoad <- function(...) {
  s3_register("base::format", "zephyr_option")
  s3_register("base::print", "zephyr_option")
  s3_register("base::print", "zephyr_options")
}
# nocov end
