#' Aggregate E4 data into 1min timesteps
#' @param x  An object read by \code{\link{read_e4}}.
#' @export
aggregate_e4_data <- function(x){
  
  
  x$EDA <- padr::thicken(x$EDA, interval = "1 min", 
                         colname = "datetime_1min") %>%
    dplyr::group_by(datetime_1min) %>%
    summarize(V1 = mean(V1, na.rm = TRUE)) %>%
    dplyr::rename(DateTime = datetime_1min)
  
  x$ACC <- padr::thicken(x$ACC, interval = "1 min", 
                         colname = "datetime_1min") %>%
    group_by(datetime_1min) %>%
    summarize(V1 = mean(V1, na.rm = TRUE),
              V2 = mean(V2, na.rm = TRUE),
              V3 = mean(V3, na.rm = TRUE),
              V4 = mean(V4, na.rm = TRUE)) %>%
    dplyr::rename(DateTime = datetime_1min)
  
  x$TEMP <- padr::thicken(x$TEMP, interval = "1 min", 
                          colname = "datetime_1min") %>%
    group_by(datetime_1min) %>%
    summarize(V1 = mean(V1, na.rm = TRUE)) %>%
    dplyr::rename(DateTime = datetime_1min)
  
  x$HR <- padr::thicken(x$HR, interval = "1 min", 
                        colname = "datetime_1min") %>%
    group_by(datetime_1min) %>%
    summarize(V1 = mean(V1, na.rm = TRUE)) %>%
    dplyr::rename(DateTime = datetime_1min)
  
  x$BVP <- NULL
  x$IBI <- NULL
  
  return(x)  
}
