

#' This function finds the peaks of an EDA signal and adds basic properties to the datafile.
#' Also, peak_end is assumed to be no later than the start of the next peak. (Is this okay??)
#' 
#' @param data:        DataFrame with EDA as one of the columns and indexed by a datetimeIndex
#' @param offset:      the number of rising samples and falling samples after a peak needed to be counted as a peak
#' @param start_WT:    maximum number of seconds before the apex of a peak that is the "start" of the peak
#' @param end_WT:      maximum number of seconds after the apex of a peak that is the "rec.t/2" of the peak, 50% of amp
#' @param thres:       the minimum microsecond change required to register as a peak, defaults as 0 (i.e. all peaks count)
#' @param sampleRate:  number of samples per second, default=8
#' @return data$peaks:               list of binary, 1 if apex of SCR
#' @return data$peak_start:          list of binary, 1 if start of SCR
#' @return data$peak_start_times:    list of strings, if this index is the apex of an SCR, it contains datetime of start of peak
#' @return data$peak_end:            list of binary, 1 if rec.t/2 of SCR
#' @return data$peak_end_times:      list of strings, if this index is the apex of an SCR, it contains datetime of rec.t/2
#' @return data$amplitude:           list of floats,  value of EDA at apex - value of EDA at start
#' @return data$max_deriv:           list of floats, max derivative within 1 second of apex of SCR
#' @export
find_peaks <- function(data, offset, start_WT, end_WT, thres=0, sampleRate = getOption("SAMPLE_RATE", 8)){
  
  data_cols <- names(data)
  
  EDA_deriv <- data$filtered_eda[2:length(data$filtered_eda)] - data$filtered_eda[1:length(data$filtered_eda)-1] 
  data$peaks <- 0 #list of 0s (add one 0 extra to match length of data)
  peak_sign <- sign(EDA_deriv)
  
  d <- diff(peak_sign)
  data$peaks[d == -2] <- 1
  
  # is the peak at index i followed by a drop (derivative = -1) of at least 'offset' nr of observations?
  peak_has_drop <- function(i){
    rle(peak_sign[(i+1):(i+1+offset+1)])$lengths[1] > offset
  }
  
  i_peaks <- which(data$peaks == 1)
  drops <- sapply(i_peaks, peak_has_drop)
  
  data$peaks[i_peaks[!drops]] <- 0
  
  # 
  # for (i in as.integer(offset) : (as.integer(length(EDA_deriv) - offset)-1)){ #-1 to make end exclusive
  #   
  #   if (peak_sign[i] == 1 & peak_sign[i + 1] < 1){
  #     data$peaks[i] <- 1
  #     
  #     for (j in 1 : (as.integer(offset)-1)){ #-1 to make end exclusive
  #       if (peak_sign[i - j] < 1 | peak_sign[i + j] > -1){
  #         data$peaks[i] <- 0
  #         break
  #       }
  #     }
  #     
  #   }
  #   
  # }
  
  
  # Finding start of peaks
  data <- add_peak_starts(data, sampleRate, start_WT, thres, EDA_deriv)
  
  # Finding the end of the peak, amplitude of peak
  data <- add_peak_ends(data, sampleRate, end_WT)
  
  data$max_deriv <- data$max_deriv * sampleRate  # now in change in amplitude over change in time form (uS/second)
  
  #set in same order as original python package
  order <- c('peaks', 'peak_start', 'peak_end', 'peak_start_times', 'peak_end_times', 'half_rise',
             'amp', 'max_deriv', 'rise_time', 'decay_time', 'SCR_width')
  data <- data[, c(data_cols, order)] 
  
  return(data)
}

add_peak_starts <- function(data, sampleRate, start_WT, thres, EDA_deriv){
  
  # Finding start of peaks
  data$peak_start <- integer(length(EDA_deriv)+1) #list of 0s (add one 0 extra to match length of data)
  data$peak_start_times <- as.POSIXct(rep(NA, nrow(data))) #list of empty dates
  data$max_deriv <- integer(nrow(data))
  data$rise_time <- integer(nrow(data))         
  
  i_peak <- which(data$peaks == 1)
  peak_sign <- sign(EDA_deriv)
  
  rise_time_for_peak <- function(i, max_lookback = sampleRate * 100){
    
    max_lookback <- min(max_lookback, i - max_lookback)
    
    r <- rle(rev(peak_sign[(i - max_lookback):i]))
    r$lengths[1]  
  }
  
  data$rise_time[i_peak] <- sapply(i_peak, rise_time_for_peak) / sampleRate
  
  # for (i in 1 : (nrow(data)-1)){  
  #   if (data$peaks[i] == 1){
  #     
  #     
  #     temp_start <- max(1, i - sampleRate) 
  #     data$max_deriv[i] <- max(EDA_deriv[temp_start:i])
  #     start_deriv <- .01 * data$max_deriv[i]
  #     
  #     found <- FALSE
  #     find_start <- i
  #     # has to peak within start_WT seconds
  #     while (found == FALSE & find_start > (i - start_WT * sampleRate)){
  #       if (EDA_deriv[find_start] < start_deriv){
  #         found <- TRUE
  #         data$peak_start[find_start] <- 1
  #         data$peak_start_times[i] <- data$DateTime[find_start] 
  #         data$rise_time[i] <- difftime(data$DateTime[i], data$peak_start_times[i], units="secs") 
  #       }
  #       find_start <- find_start - 1
  #     }
  #     # If we didn't find a start
  #     if (found == FALSE){
  #       data$peak_start[i - start_WT * sampleRate] <- 1
  #       data$peak_start_times[i] <- data$DateTime[i - start_WT * sampleRate]
  #       data$rise_time[i] <- start_WT
  #     }
  #     # Check if amplitude is too small
  #     if (thres > 0 & (data$EDA[i] - data$EDA[data$DateTime == data$peak_start_times[i]]) < thres){
  #       data$peaks[i] <- 0
  #       data$peak_start[i] <- 0
  #       data$peak_start_times[i] = as.POSIXct(NA) #set empty date
  #       data$max_deriv[i] <- 0
  #       data$rise_time[i] <- 0
  #     }
  #   }
  # }
  
  
  return(data)
}

add_peak_ends <- function(data, sampleRate, end_WT){
  
  
  
  # 
  # Finding the end of the peak, amplitude of peak
  data$peak_end <- integer(nrow(data))
  data$peak_end_times <- as.POSIXct(rep(NA, nrow(data))) #list of empty dates
  data$amp <- integer(nrow(data))
  data$decay_time <- integer(nrow(data))
  data$half_rise <- as.POSIXct(rep(NA, nrow(data))) #list of empty dates
  data$SCR_width <- integer(nrow(data))

  # for (i in 1:(length(data$peaks)-1)){
  #   if (data$peaks[i] == 1){
  #     peak_amp <- data$EDA[i]
  #     start_amp <- data$EDA[data$DateTime == data$peak_start_times[i]]
  #     data$amp[i] <- peak_amp - start_amp
  #     
  #     half_amp <- data$amp[i] * .5 + start_amp
  #     
  #     found <- FALSE
  #     find_end <- i
  #     # has to decay within end_WT seconds
  #     while (found == FALSE & find_end < (i + end_WT * sampleRate) & find_end < (length(data$peaks)-1)){
  #       if (data$EDA[find_end] < half_amp){
  #         found <- TRUE
  #         data$peak_end[find_end] <- 1
  #         data$peak_end_times[i] <- data$DateTime[find_end]
  #         data$decay_time[i] <- difftime(data$peak_end_times[i], data$DateTime[i], units="secs") 
  #         
  #         # Find width
  #         find_rise <- i
  #         found_rise <- FALSE
  #         while (found_rise == FALSE){
  #           if (data$EDA[find_rise] < half_amp){
  #             found_rise <- TRUE
  #             data$half_rise[i] <- data$DateTime[find_rise]
  #             data$SCR_width[i] <- difftime(data$peak_end_times[i], data$DateTime[find_rise], units="secs")
  #           }
  #           find_rise <- find_rise - 1
  #         }
  #       } else if (data$peak_start[find_end] == 1){
  #         found <- TRUE
  #         data$peak_end[find_end] <- 1
  #         data$peak_end_times[i] <- data$DateTime[find_end]
  #       }
  #       find_end <- find_end + 1
  #     }
  #     # If we didn't find an end
  #     if (found == FALSE){
  #       min_index <- which.min(data$EDA[i:(i + end_WT * sampleRate)])
  #       data$peak_end[i + min_index] <- 1
  #       data$peak_end_times[i] <- data$DateTime[i + min_index]
  #     }
  #   }
  # }
  return(data)
}


#' Write peak features
#' @param data_with_peaks
#' @param outfile
#' @export
write_peak_features <- function(data_with_peaks, outfile){
  
  featureData <- data_with_peaks[data_with_peaks$peaks==1,][c('DateTime', 'EDA','rise_time','max_deriv','amp','decay_time','SCR_width')]
  # To write all filtered data, not only information on the peaks, use this line instead
  #featureData <- data_with_peaks
  
  # Replace 0s with NA, this is where the 50% of the peak was not found, too close to the next peak
  featureData[, c('SCR_width','decay_time')][featureData[, c('SCR_width','decay_time')] == 0] <- NA
  featureData['AUC'] <- featureData['amp'] * featureData['SCR_width']
  
  # Warning: Write table writes time as is (in System.timezone), not as UTC 
  write.table(featureData, outfile, sep=';', dec = ",", row.names = FALSE)
}


# ############ MAIN CODE ######################
#
# fullOutputPath = "features.csv"
#
# #for testing (defaults)
# #offset <- 1
# #thres <- 0.02
# #start_WT <- 4
# #end_WT <- 4
#
# #settings Peter de Looff
# offset = 1
# thres = 0.005
# start_WT = 4
# end_WT = 4
#
# print(paste("Finding peaks in file", filepath, "using the following parameters"))
# print(paste0("Offset: ", offset, "; Minimum peak amplitude: ", thres, "; Max rise time (s): ", start_WT,"; Max decay time (s): ", end_WT))
#
# data_with_peaks <- find_peaks(eda_data, offset*SAMPLE_RATE, start_WT, end_WT, thres, SAMPLE_RATE)
#
# write_peak_features(data_with_peaks, fullOutputPath)
# print(paste0("Features computed and saved to ", fullOutputPath))
#
# ########### PLOTTING #########################
#
# plot(peakData$DateTime, eda_data$filtered_eda, type='l', col='black')
# #peaks only
# foundPeaks <- peakData[peakData$peaks==1,]
# abline(v=foundPeaks$DateTime, lwd = 2, col = rgb(0, 1, 0, alpha=0.5))


