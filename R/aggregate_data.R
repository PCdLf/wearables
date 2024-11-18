#' Aggregate data into timesteps
#' @param x An object read by \code{\link{read_e4}}, \code{\link{read_embrace_plus}} or \code{\link{read_nowatch}}.
#' @param interval The interval to aggregate the data. Default is 1 min.
#' @export
aggregate_data <- function(x, interval = "1 min") {
  
  for (name in names(x)) {
    
    if (nrow(x[[name]]) == 0 || !any(c("DateTime", "datetime_1min") %in% colnames(x[[name]]))) {
      next
    }
    
    x[[name]] <- padr::thicken(x[[name]],
                               interval = interval,
                               colname = "datetime_1min"
    ) %>%
      dplyr::group_by(datetime_1min) %>%
      dplyr::summarise(across(where(is.numeric), mean)) %>%
      dplyr::rename(DateTime = datetime_1min)
  }
  
  x$BVP <- NULL
  x$IBI <- NULL
  x$GY <- NULL
  x$steps <- NULL
  x$systolic_peaks <- NULL
  
  return(x)
  
}

#' Aggregate E4 data into timesteps
#' @rdname aggregate_data
#' @export
aggregate_e4_data <- function(x, interval = "1 min") {
  
  if (is.null(x$EDA) && is.null(x$TEMP) && is.null(x$HR)) {
    warning("Data not found. Did you run rbind_e4()?")
  }
  
  x <- aggregate_data(x, interval = interval)
  
  return(x)
  
}

#' Aggregate Embrace Plus data into timesteps
#' @rdname aggregate_data
#' @export
aggregate_embrace_plus_data <- function(x, interval = "1 min") {
  
  if (is.null(x$EDA) && is.null(x$TEMP) && is.null(x$HR)) {
    warning("Data not found. Did you run rbind_embrace_plus()?")
  }
  
  x <- aggregate_data(x, interval = interval)
  
  return(x)
}

#' Aggregate Nowatch data into timesteps
#' @rdname aggregate_data
#' @export
aggregate_nowatch_data <- function(x, interval = "1 min") {
  
  if (is.null(x$ACT) && is.null(x$TEMP) && is.null(x$HR)) {
    warning("Data not found. Did you use read_nowatch()?")
  }
  
  x <- aggregate_data(x, interval = interval)
  
  return(x)
}

