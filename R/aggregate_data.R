#' Aggregate data into 1min timestamps
#' @param x An object read by \code{\link{read_e4}} or \code{\link{read_embrace_plus}}.
#' @export
aggregate_data <- function(x) {
  
  if ("EDA" %in% names(x)) {
    
    x$EDA <- padr::thicken(x$EDA,
                           interval = "1 min",
                           colname = "datetime_1min"
    ) %>%
      dplyr::group_by(datetime_1min) %>%
      summarize(EDA = mean(EDA)) %>%
      dplyr::rename(DateTime = datetime_1min)
    
  }
  
  if ("ACC" %in% names(x)) {
    
    x$ACC <- padr::thicken(x$ACC,
                           interval = "1 min",
                           colname = "datetime_1min"
    ) %>%
      group_by(datetime_1min) %>%
      summarize(
        x = mean(x),
        y = mean(y),
        z = mean(z),
        a = mean(a)
      ) %>%
      dplyr::rename(DateTime = datetime_1min)
    
  }
  
  if ("TEMP" %in% names(x)) {
    
    x$TEMP <- padr::thicken(x$TEMP,
                            interval = "1 min",
                            colname = "datetime_1min"
    ) %>%
      group_by(datetime_1min) %>%
      summarize(TEMP = mean(TEMP)) %>%
      dplyr::rename(DateTime = datetime_1min)
    
  }
  
  if ("HR" %in% names(x)) {
    
    x$HR <- padr::thicken(x$HR,
                          interval = "1 min",
                          colname = "datetime_1min"
    ) %>%
      group_by(datetime_1min) %>%
      summarize(HR = mean(HR)) %>%
      dplyr::rename(DateTime = datetime_1min)
    
  }
  
  x$BVP <- NULL
  x$IBI <- NULL
  x$GY <- NULL
  x$steps <- NULL
  x$systolic_peaks <- NULL
  
  return(x)
  
}

#' Aggregate E4 data into 1min timesteps
#' @rdname aggregate_data
#' @export
aggregate_e4_data <- function(x) {
  
  if (is.null(x$EDA)) {
    warning("Data not found. Did you run rbind_e4()?")
  }
  
  x <- aggregate_data(x)
  
  return(x)
  
}

#' Aggregate Embrace Plus data into 1min timesteps
#' @rdname aggregate_data
#' @export
aggregate_embrace_plus_data <- function(x) {
  
  if (is.null(x$EDA)) {
    warning("Data not found. Did you run rbind_embrace_plus()?")
  }
  
  x <- aggregate_data(x)
  
  return(x)
}

