#' Filter all four datasets for a Datetime start + end
#' @param data Object read with \code{\link{read_e4}}
#' @param start Start Datetime (posixct)
#' @param end End Datetime (posixct)
#' @export
#' @importFrom dplyr .data
filter_e4data_datetime <- function(data, start, end) {
  data$IBI$datetime <- lubridate::force_tz(data$IBI$DateTime, "UTC")
  data$EDA$datetime <- lubridate::force_tz(data$EDA$DateTime, "UTC")
  data$ACC$datetime <- lubridate::force_tz(data$ACC$DateTime, "UTC")
  data$TEMP$datetime <- lubridate::force_tz(data$TEMP$DateTime, "UTC")
  data$HR$datetime <- lubridate::force_tz(data$HR$DateTime, "UTC")

  data$IBI <- dplyr::filter(
    data$IBI,
    .data$datetime >= start,
    .data$datetime <= end
  )
  data$EDA <- dplyr::filter(
    data$EDA,
    .data$datetime >= start,
    .data$datetime <= end
  )
  data$ACC <- dplyr::filter(
    data$ACC,
    .data$datetime >= start,
    .data$datetime <= end
  )
  data$TEMP <- dplyr::filter(
    data$TEMP,
    .data$datetime >= start,
    .data$datetime <= end
  )
  data$HR <- dplyr::filter(
    data$HR,
    .data$datetime >= start,
    .data$datetime <= end
  )

  return(data)
}
