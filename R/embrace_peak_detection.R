# library(dplyr)
# library(ggplot2)
# 
# head(embrace_test[[1]]$systolic_peaks$PEAKS) 
# 
# # Custom function to format DateTime with nanosecond precision
# format_datetime_ns <- function(datetime, nanoseconds) {
#   sprintf("%s.%09d", format(datetime, "%Y-%m-%d %H:%M:%OS"), nanoseconds)
# }
# 
# nano_df <- embrace_test[[1]]$systolic_peaks %>% 
#   dplyr::mutate(
#     unixseconds = PEAKS %/% 1e9,
#     nanoseconds = PEAKS %% 1e9,
#     DateTime = as.POSIXct(unixseconds, origin = "1970-01-01", tz = "UTC"),
#     DateTimeNS = format_datetime_ns(DateTime, nanoseconds)
#   )
# 
# head(nano_df)
# 
# # SECOND PIECE OF CODE
# # Ensure the required libraries are loaded
# library(dplyr)
# 
# # Custom function to format DateTime with nanosecond precision
# format_datetime_ns <- function(datetime, nanoseconds) {
#   sprintf("%s.%09d", format(datetime, "%Y-%m-%d %H:%M:%OS"), nanoseconds)
# }
# 
# # Your provided code to create the dataframe
# nano_df <- embrace_test[[1]]$systolic_peaks %>% 
#   dplyr::mutate(
#     unixseconds = PEAKS %/% 1e9,
#     nanoseconds = PEAKS %% 1e9,
#     DateTime = as.POSIXct(unixseconds, origin = "1970-01-01", tz = "UTC"),
#     DateTimeNS = format_datetime_ns(DateTime, nanoseconds)
#   )
# 
# # Calculate the differences and create the beat times vector
# beat_times <- nano_df %>%
#   dplyr::mutate(
#     TimeDiff = c(0, diff(PEAKS)) / 1e9  # Calculate differences and convert nanoseconds to seconds
#   ) %>%
#   dplyr::mutate(
#     BeatTime = cumsum(TimeDiff)  # Cumulative sum to get the time between beats
#   ) %>%
#   dplyr::pull(BeatTime)  # Extract the BeatTime column as a vector
# 
# # Print the first few values to check the result
# print(head(beat_times, 10))
# 
# embrace_test[[1]]$systolic_peaks$PEAKS[2]-embrace_test[[1]]$systolic_peaks$PEAKS[1]
# 
# as.POSIXlt(nano_df$DateTime[1], origin = "1970-01-01", tz = "UTC")
# 
# 
# # Plotting the data using ggplot2
# ggplot(nano_df, aes(x = DateTime, y = nanoseconds)) +
#   geom_point() +
#   labs(title = "Beats Detected with Nanosecond Precision",
#        x = "DateTime",
#        y = "Nanoseconds") +
#   theme_minimal()
# 
# # Plotting the data using ggplot2
# ggplot(nano_df, aes(x = DateTime, y = nanoseconds)) +
#   geom_point() +
#   geom_line() +  # Adding lines to connect the points
#   labs(title = "Beats Detected with Nanosecond Precision",
#        x = "DateTime",
#        y = "Nanoseconds") +
#   theme_minimal()
# 
# 
# 
# 
# library(tidyverse)
# 
# # Example RR interval data in milliseconds (ms)
# rr_intervals <- c(800, 810, 795, 815, 820, 805, 790, 805, 810, 815, 800, 795)
# 
# # Convert RR intervals to seconds for spectral analysis
# rr_intervals_sec <- rr_intervals / 1000
# 
# # Interpolate the RR intervals to create a continuous time series
# rr_time <- seq(0, length(rr_intervals_sec) - 1, by = 1)
# rr_continuous <- approx(rr_time, rr_intervals_sec, n = 1024)$y
# 
# # Perform FFT to convert to frequency domain
# rr_fft <- fft(rr_continuous)
# 
# # Calculate Power Spectral Density (PSD)
# rr_psd <- abs(rr_fft)^2
# 
# # Frequency axis
# n <- length(rr_psd)
# freq <- (0:(n-1)) / n
# 
# # Plot the Power Spectral Density
# plot(freq, rr_psd, type = "l", xlab = "Frequency (Hz)", ylab = "Power",
#      main = "Power Spectral Density of RR Intervals")
# 
# 
# 
# 
