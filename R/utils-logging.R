#' @noRd
.log_info <- function(...) {
  if (requireNamespace("futile.logger", quietly = TRUE)) {
    futile.logger::flog.info(...)
  } else {
    message(...)
  }
}