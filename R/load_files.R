###################
# Code to process EDA data
###################

#' Process EDA data
#' @param eda_data Data read with \code{\link{read_e4}}
#' @export
process_eda <- function(eda_data){
  
  # Make sure data has a sample rate of 8Hz
  eda_data <- upsample_data_to_8Hz(eda_data)
  
  # Get the filtered data using a low-pass butterworth filter (cutoff:1hz, sampling frequency:8hz, order:6)
  eda_data$filtered_eda <-  as.numeric(butter_lowpass_filter(eda_data$EDA, 1.0, 8, 6))
  
  return(eda_data)
}

######################
# code to bring signal to 8Hz (currently only supports upsampling)
######################
#' Upsample EDA data to 8 Hz
#' @param eda_data Data read with \code{\link{read_e4}}
#' @importFrom stats approx
#' @export
upsample_data_to_8Hz <- function(eda_data){
  # Upsample
  start <- eda_data$DateTime[[1]]
  end <- eda_data$DateTime[length(eda_data$DateTime)]
  time_sequence_8Hz <- seq(from = start, to = end, units = "seconds", by = .125)
  #python: data = data.resample("125L").mean()
  
  # Interpolate all empty values
  interpolated <- approx(eda_data$DateTime, eda_data$EDA, xout = time_sequence_8Hz)
  data <- data.frame(DateTime = interpolated[1], EDA = interpolated[2])
  names(data)[1] <- "DateTime"
  names(data)[2] <- "EDA"
  #python: data = interpolateEmptyValues(data)
  return(data)
}

######################
# code to filter signal
#https://stackoverflow.com/questions/7105962/how-do-i-run-a-high-pass-or-low-pass-filter-on-data-points-in-r
######################

#' @importFrom signal butter
butter_lowpass_filter <- function(data, cutoff, sampling_frequency, order = 5){
  # Filtering Helper functions
  nyquist_frequency <- 0.5 * sampling_frequency
  normal_cutoff <- cutoff / nyquist_frequency
  butter_filter <- signal::butter(order, normal_cutoff, type = "low", plane = "z") #"z" for a digital filter
  #python: c(b, a) = scisig.butter(order, normal_cutoff, btype='low', analog=False)
  # Apply filter
  y <- signal::filter(butter_filter, data)
  #python: y = scisig.lfilter(b, a, data)
  return (y)
}

