#' RMSSD calculation 
#' 
#' Calculation of RMSSD over 1 minute time periods for plotting
#' 
#' @param IBIdata Uses the IBI data frame as created by \code{\link{read_e4}}
#' @export
#' @importFrom varian rmssd
#' @importFrom dplyr .data
calculate_RMSSD <- function(IBIdata){
  #heart rate variability from inter beat intervals
  #https://www.sciencedirect.com/science/article/pii/S0735109797005548
  #https://www.hrv4training.com/blog/heart-rate-variability-normal-values
  
  # Successive RR interval differences (in milliseconds)
  IBI <- IBIdata %>% dplyr::mutate(IBI_ms = (IBI * 1000))
  
  # Calculates the root mean square of successive differences (RMSSD)
  RMSSD <- IBI %>%
    #group per 5 minute interval
    dplyr::group_by(bin = cut(.data$DateTime, "1 min")) %>%
    #when more than one datapoint
    dplyr::filter(dplyr::n() > 1) %>%
    #calculate root mean squared error of difference scores
    #rmssd = root(average(difference between this and previous IBI)square)
    dplyr::summarize(RMSSD = rmssd(.data$IBI_ms)) %>%
    #cast grouping to a timestamp
    dplyr::mutate(time = as.POSIXct(as.character(.data$bin)))
  
  return(RMSSD)
}