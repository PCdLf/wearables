#' Filter data according to a datetime start and end
#' @param data Object read with \code{\link{read_e4}} or \code{\link{read_embrace_plus}}
#' @param start Start Datetime (posixct)
#' @param end End Datetime (posixct)
#' @export
#' @importFrom dplyr .data
filter_datetime <- function(data, start, end) {
  if ("IBI" %in% names(data)) {
    data$IBI$datetime <- lubridate::force_tz(data$IBI$DateTime, "UTC")
    data$IBI <- dplyr::filter(
      data$IBI,
      .data$datetime >= start,
      .data$datetime <= end
    )
  }
  
  if ("EDA" %in% names(data)) {
    data$EDA$datetime <- lubridate::force_tz(data$EDA$DateTime, "UTC")
    data$EDA <- dplyr::filter(
      data$EDA,
      .data$datetime >= start,
      .data$datetime <= end
    )
  }
  
  if ("ACC" %in% names(data)) {
    data$ACC$datetime <- lubridate::force_tz(data$ACC$DateTime, "UTC")
    data$ACC <- dplyr::filter(
      data$ACC,
      .data$datetime >= start,
      .data$datetime <= end
    )
  }
  
  if ("TEMP" %in% names(data)) {
    data$TEMP$datetime <- lubridate::force_tz(data$TEMP$DateTime, "UTC")
    data$TEMP <- dplyr::filter(
      data$TEMP,
      .data$datetime >= start,
      .data$datetime <= end
    )
  }
  
  if ("HR" %in% names(data)) {
    data$HR$datetime <- lubridate::force_tz(data$HR$DateTime, "UTC")
    data$HR <- dplyr::filter(
      data$HR,
      .data$datetime >= start,
      .data$datetime <= end
    )
  }
  
  return (data)

}

#' Filter all four datasets for a Datetime start + end
#' @rdname filter_datetime
#' @export
filter_e4data_datetime <- function(data, start, end) {

  filtered_data <- filter_datetime(data, start, end)

  return(filtered_data)
}

#' Filter all datasets for a Datetime start + end
#' @rdname filter_datetime
#' @export
filter_embrace_plus_data_timestamp <- function(data, start, end) {
  
  filtered_data <- filter_datetime(data, start, end)
  
  return(filtered_data)
}
