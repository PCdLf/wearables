#' Show class of object
#' @description Returns 'object of class'
#' @param x An e4 data list
#' @param \dots Further arguments currently ignored.
#' @export
print.e4data <- function(x, ...) {
  cat("This is an object of class 'e4data'.\n")
  cat("Datasets included:", paste(names(x), collapse = ", "), "\n")
}

#
# summary.e4data <- function(object, ...){
#
# }

# plot.e4data <- function(x, ...){
#
# }
