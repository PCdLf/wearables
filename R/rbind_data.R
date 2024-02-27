#' Row-bind datasets
#' @details Serves as an internal function that is called by
#'  `rbind_e4`, `rbind_embrace_plus` and `rbind_nowatch`. It takes a list 
#'  of data.frames and row-binds them together.
#' @param data List object containing data.frames
#' @importFrom dplyr bind_rows
#' @keywords internal
rbind_data <- function(data) {
  out <- list()

  nms <- names(data[[1]])

  for (name in nms) {
    # retrieve data
    dat <- lapply(data, "[[", name)
    out[[name]] <- bind_rows(dat)
  }

  out
}

#' Row-bind E4 datasets
#' 
#' @description This function takes a list of E4 datasets and row-binds them
#' together.
#' @param data An object read in by `\code{\link{read_e4}}
#' @export
rbind_e4 <- function(data) {
  rbind_data(data)
}

#' Row-bind Embrace Plus datasets
#' 
#' @description This function takes a list of Embrace Plus datasets and row-binds 
#' them together.
#' @param data An object read in by \code{\link{read_embrace_plus}}
#' @export
rbind_embrace_plus <- function(data) {
  rbind_data(data)
}

#' Row-bind NOWATCH datasets
#' 
#' @description This function takes a list of NOWATCH datasets and row-binds them
#' together.
#' @param data An object read in by \code{\link{read_nowatch}}
rbind_nowatch <- function(data) {
  rbind_data(data)
}

