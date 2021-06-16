#' Electrodermal activity signal derivative 
#' 
#' Finds the first derivatives of the eda signal
#' 
#' @param eda eda vector
get_eda_deriv <- function(eda){
  eda[2:length(eda)] - eda[1:(length(eda)-1)]   
}


#' et the eda apex of the signal
#' 
#' finds the apex of electrodermal activity eda signal 
#'   within an optional time window
#'   
#' @param eda_deriv uses the eda derivative to find the apex
#' @param offset minimum number of downward measurements after the apex,
#'   in order to be considered a peak (default 1 means no restrictions)
get_apex <- function(eda_deriv, offset = 1){
  peak_has_drop <- function(i){
    length_drop <- rle(peak_sign[i:(i+offset-1)])$lengths[1]
    length_drop >= offset
  }
  
  peak_sign <- sign(eda_deriv)
  apex <- integer(length(peak_sign) + 1)
  apex[c(FALSE, diff(peak_sign) == -2)] <- 1

  i_apex <- which(apex == 1)
  has_drops <- sapply(i_apex, peak_has_drop)
  
  apex[i_apex[!has_drops]] <- 0
  apex
}

#' Rise time of peaks
#' 
#' Calculates the rise time of all peaks
#' 
#' @param eda_deriv first derivative of signal
#' @param apices apex status per measurement (0 or 1)
#' @param sample_rate sample rate of the signal 
#' @param start_WT window within which to look for rise time (in seconds)
get_rise_time <- function(eda_deriv, apices, sample_rate, start_WT){ 
  get_rise_events_for_apex <- function(i){
    lookback_i <- max(1, i - max_lookback)
    
    r <- rle(rev(peak_sign[lookback_i:i - 1]))
    r$lengths[1]  
  }
  
  peak_sign <- sign(eda_deriv)
  max_lookback <- sample_rate * start_WT
  
  rise_time <- numeric(length(apices))         
  
  i_apex <- which(apices == 1)

  rise_time[i_apex] <- 
    sapply(i_apex, get_rise_events_for_apex) / 
    sample_rate
  
  rise_time
}

#' Start of peaks
#' 
#' Provide info for each measurement whether it is the start of a peak (0 or 1)
#'  
#' @param data df with peak info
#' @param sample_rate sample rate of the signal 
get_peak_start <- function(data, sample_rate){
  i_apex <- which(data$peaks == 1)
  peak_start <- integer(nrow(data)) 
  length_rise_events <- sample_rate * data$rise_time[i_apex]
  peak_start[i_apex - length_rise_events] <- 1
  peak_start
}

#' Small peaks removal
#' 
#' Remove peaks with a small rise from start to apex are removed
#' 
#' @param data df with info on peaks
#' @param thres threshold of amplitude difference in order to be removed
#'   (default 0 means no removals)
remove_small_peaks <- function(data, thres = 0){
  if(thres > 0){
    i_apex <- which(data$peaks == 1)
    i_peak_start <- which(data$peak_start ==  1)
    i_to_remove <- data$filtered_eda[i_apex] - data$filtered_eda[i_peak_start] < thres
    
    data$peaks[i_apex][i_to_remove] <- 0
    data$rise_time[i_apex][i_to_remove] <- 0
    data$peak_start[i_peak_start][i_to_remove] <- 0
  }
  data
}

#' Peak start times
#' 
#' Get the start times of the peaks
#' 
#' @param data df with peak info
get_peak_start_times <- function(data){
  i_apex <- which(data$peaks == 1)
  i_peak_start <- which(data$peak_start ==  1)
  peak_start_times <- as.POSIXct(rep(NA, nrow(data)))
  peak_start_times[i_apex] <- data$DateTime[i_peak_start]
  peak_start_times
}

#' Maximum derivative
#' 
#' Get the largest slope before apex, interpolated to seconds
#' 
#' @param data df with info on the peaks
#' @param eda_deriv derivative of the signal
#' @param sample_rate sample rate of the signal
get_max_deriv <- function(data, eda_deriv, sample_rate){
  
  get_max_deriv_for_event <- function(i){
    max(eda_deriv[max(1, i - sample_rate):i])
  }
  
  i_apex <- which(data$peaks == 1)
  max_deriv <- numeric(nrow(data))
  
  max_deriv[i_apex] <-
    sapply(i_apex, get_max_deriv_for_event) *
    sample_rate
  
  max_deriv
}

#' Peak amplitude
#' 
#' Get the amplitude of the peaks
#' 
#' @param data df with peak info
get_amp <- function(data){
  i_apex <- which(data$peaks == 1)
  i_peak_start <- which(data$peak_start ==  1)
  amp <- numeric(nrow(data))
  apex_amp <- data$filtered_eda[i_apex]
  start_amp <- data$filtered_eda[i_peak_start]
  amp[i_apex] <- apex_amp - start_amp
  amp
}

#' Half peak amp
#'
#' Get the amplitude value halfway between peak start and apex
#' 
#' @param data df with peak info
#' @param i apex index
get_half_amp <- function(data, i){
  apex_amp <- data$filtered_eda[i] 
  amp_diff <- data$amp[i]
  half_amp <- apex_amp - .5 * amp_diff
  half_amp
}

#' Peak end
#' 
#' Find the end of the peaks, with some restrictions on the search
#' 
#' @param data df with peak info
#' @param max_lookahead max distance from apex to search for end
#' @importFrom utils tail
get_peak_end <- function(data, max_lookahead){
  get_i_end_per_apex <- function(i, i_max_peak_end){ 
    half_amp <- get_half_amp(data, i)
    i_lookahead <- min(i_max_peak_end, i + max_lookahead)
    amps_ahead <- data$filtered_eda[(i + 1):(i_lookahead)]
    length_peak_end <- which(amps_ahead < half_amp)[1]
    
    if(is.na(length_peak_end)){
      i + which.min(amps_ahead)
    } else {
      i + length_peak_end
    }
  }
  
  i_apex <- which(data$peaks == 1)
  i_peak_start <- which(data$peak_start == 1)
  i_next_peak_start <- tail(i_peak_start, -1)
  i_max_peak_end <- c(i_next_peak_start - 1, nrow(data))

  i_peak_end <- mapply(get_i_end_per_apex, i_apex, i_max_peak_end)
  
  peak_end <- integer(nrow(data))
  peak_end[i_peak_end] <- 1
  
  peak_end
}

#' Peak end times
#' 
#' Get the end timstamp of the peaks
#' 
#' @param data df with peak info
get_peak_end_times <- function(data){
  peak_end_times <- as.POSIXct(rep(NA, nrow(data)))
  i_apex <- which(data$peaks == 1)
  i_peak_end <- which(data$peak_end == 1)
  peak_end_times[i_apex] <- data$DateTime[i_peak_end]
  peak_end_times
}

#' Decaying peaks
#' 
#' Identify peaks with a decent decay (at least half the amplitude of rise)
#' 
#' @param data df with peak info
get_i_apex_with_decay <- function(data){
  i_apex <- which(data$peaks == 1)
  i_peak_end <- which(data$peak_end == 1)
  half_amp <- get_half_amp(data, i_apex)
  has_decay <- data$filtered_eda[i_peak_end] < half_amp
  i_apex[has_decay]
}

#' Decay time
#' 
#' Get the time (in seconds) it takes to decay for each peak
#' 
#' @param data df with peak info
#' @param i_apex_with_decay indexes of relevant peaks
get_decay_time <- function(data, i_apex_with_decay){
  decay_time <- numeric(nrow(data))
  decay_time[i_apex_with_decay] <- as.numeric(difftime(
    data$peak_end_times[i_apex_with_decay], 
    data$DateTime[i_apex_with_decay], 
    units="secs"
  ))
  decay_time
}

#' Half rise time
#' 
#' Get the time (in seconds) it takes to get to halfway the rise in a peak
#' 
#' @param data df with peak info
#' @param i_apex_with_decay relevant apices
get_half_rise <- function(data, i_apex_with_decay){
  
  get_i_half_rise <- function(i_peak_start, i_apex){
    half_amp <- data$filtered_eda[i_apex] - .5 * data$amp[i_apex]
    is_below_amp <- data$filtered_eda[(i_apex - 1):i_peak_start] < half_amp
    i_apex - which(is_below_amp)[1]
  }
  
  i_apex <- which(data$peaks == 1)
  has_decay <- i_apex %in% i_apex_with_decay
  i_peak_start_with_decay <- which(data$peak_start ==  1)[has_decay]
  
  i_half_rise <- mapply(get_i_half_rise, i_peak_start_with_decay, i_apex_with_decay)
  half_rise <- as.POSIXct(rep(NA, nrow(data)))
  half_rise[i_apex_with_decay] <- data$DateTime[i_half_rise]
  half_rise
}

#' Peak width
#' 
#' Get the width of the peak (in seconds, from halfway the rise until the end)
#' 
#' @param data df with peak info
#' @param i_apex_with_decay relevant apices
get_SCR_width <- function(data, i_apex_with_decay){
  SCR_width <- numeric(nrow(data))
  SCR_width[i_apex_with_decay] <- as.numeric(difftime(
    data$peak_end_times[i_apex_with_decay], 
    data$half_rise[i_apex_with_decay], 
    units="secs"
  ))
  SCR_width
}

#' Function to find peaks of an EDA datafile
#' @description This function finds the peaks of an EDA signal and adds 
#'   basic properties to the datafile.
#' @details Also, peak_end is assumed to be no later than the start of the next peak. 
#'   Is that OK?
#' @param data DataFrame with EDA as one of the columns and indexed by a datetimeIndex
#' @param offset the number of rising seconds and falling seconds after a peak needed to be counted as a peak
#' @param start_WT maximum number of seconds before the apex of a peak that is the "start" of the peak
#' @param end_WT maximum number of seconds after the apex of a peak that is the "end" of the peak 50 percent of amp
#' @param thres the minimum microsecond change required to register as a peak, defaults as .02
#' @param sample_rate number of samples per second, default=8
#' @return data frame with several columns
#'   peaks               1 if apex
#'   peak_start          1 if start of peak
#'   peak_end            1 if end of preak
#'   peak_start_times    if apex then corresponding start timestamp
#'   peak_end_times      if apex then corresponding end timestamp
#'   half_rise           if sharp decaying apex then time to halfway point in rise
#'   amp                 if apex then value of EDA at apex - value of EDA at start
#'   max_deriv           if apex then max derivative within 1 second of apex
#'   rise_time           if apex then time from start to apex
#'   decay_time          if sharp decaying apex then time from apex to end
#'   SCR_width           if sharp decaying apex then time from half rise to end
#' @export
find_peaks <- function(data, offset = 1, start_WT = 4, end_WT = 4, thres = .02, 
                       sample_rate = getOption("SAMPLE_RATE", 8)){ 
  
  offset <- offset * sample_rate
  old_col_names <- names(data)
  
  eda_deriv <- get_eda_deriv(data$filtered_eda)
  
  data$peaks <- get_apex(eda_deriv, offset)
  data$rise_time <- get_rise_time(eda_deriv, data$peaks, sample_rate, start_WT)
  data$peak_start <- get_peak_start(data, sample_rate)
  
  data <- remove_small_peaks(data, thres)
  
  data$peak_start_times <- get_peak_start_times(data)
  data$max_deriv <- get_max_deriv(data, eda_deriv, sample_rate)
  data$amp <- get_amp(data)
  data$peak_end <- get_peak_end(data, end_WT * sample_rate)
  data$peak_end_times <- get_peak_end_times(data)
  
  i_apex_with_decay <- get_i_apex_with_decay(data)
  
  data$decay_time <- get_decay_time(data, i_apex_with_decay)
  data$half_rise <- get_half_rise(data, i_apex_with_decay)
  data$SCR_width <- get_SCR_width(data, i_apex_with_decay)
  
  new_col_names_ordered <- c('peaks', 'peak_start', 'peak_end',
                             'peak_start_times', 'peak_end_times',
                             'half_rise', 'amp', 'max_deriv',
                             'rise_time', 'decay_time', 'SCR_width')
  data <- data[, c(old_col_names, new_col_names_ordered)]
  
  # Remove rows without peaks
  featureData <- data[data$peaks==1,][c('DateTime', 'EDA','rise_time','max_deriv','amp','decay_time','SCR_width')]
  
  # Replace 0s with NA, this is where the 50 percent of the peak was not found, too close to the next peak
  featureData[, c('SCR_width','decay_time')][featureData[, c('SCR_width','decay_time')] == 0] <- NA
  featureData['AUC'] <- featureData['amp'] * featureData['SCR_width']
  
  
  featureData
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


