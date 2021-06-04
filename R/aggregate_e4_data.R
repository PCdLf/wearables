#' Aggregate E4 data into 1min timesteps
#' @param x  An object read by \code{\link{read_e4}}.
#' @export
aggregate_e4_data <- function(x){
  
  datetime_1min <- EDA <- y <- z <- a <- TEMP <- HR <- NULL 
  
  x$EDA <- padr::thicken(x$EDA, interval = "1 min", 
                         colname = "datetime_1min") %>%
    dplyr::group_by(datetime_1min) %>%
    summarize(EDA = mean(EDA)) %>%
    dplyr::rename(DateTime = datetime_1min)
  
  x$ACC <- padr::thicken(x$ACC, interval = "1 min", 
                         colname = "datetime_1min") %>%
    group_by(datetime_1min) %>%
    summarize(x = mean(x),
              y = mean(y),
              z = mean(z),
              a = mean(a)) %>%
    dplyr::rename(DateTime = datetime_1min)
  
  x$TEMP <- padr::thicken(x$TEMP, interval = "1 min", 
                          colname = "datetime_1min") %>%
    group_by(datetime_1min) %>%
    summarize(TEMP = mean(TEMP)) %>%
    dplyr::rename(DateTime = datetime_1min)
  
  x$HR <- padr::thicken(x$HR, interval = "1 min", 
                        colname = "datetime_1min") %>%
    group_by(datetime_1min) %>%
    summarize(HR = mean(HR)) %>%
    dplyr::rename(DateTime = datetime_1min)
  
  x$BVP <- NULL
  x$IBI <- NULL
  
  return(x)  
}
