
#' Force character datetime variable ("yyyy-mm-dd hh:mm:ss") to system timezone
#' @param time Datetime variable ("yyyy-mm-dd hh:mm:ss")
#' @export
char_clock_systime <- function (time){
  force_tz(lubridate::ymd_hms(time), tz = Sys.timezone())
}


#' Filter all six datasets for a Datetime start + end
#' @description A function to determine how many intervals should be
#' created. Supports the \code{\link{filter_e4data_datetime_filecut}} function
#' The question is at what time do you want the filecut to start, what should be 
#' the period that you want separate files for, and what should the interval be?
#' @param time_start User input start time in the character format 
#' "yyyy-mm-dd hh:mm:ss" / e.g., "2019-11-27 08:32:00". Where do you want the file cut to start?
#' @param time_end User input end time (same format as time_start)
#' @param interval # Interval: User input interval (in minutes/ e.g., 5)
#' What is the duration of the interval you want to divide the period into? 
#' For example, the paper by de Looff et al. (2019) uses 5 minute intervals over
#' a 30 minute period preceding aggressive behavior. The 5 minute interval is 
#' chosen as for the calculation of some of the heart rate variability parameters 
#' one needs at least 5 minutes of data. 
#' @export
#' @importFrom lubridate ymd_hms
e4_filecut_intervals <- function(time_start, time_end, interval){
  
  # This should also be put in a separate box in the app
  user_inp_time <- lubridate::ymd_hms(time_start, tz = Sys.timezone())
  
  n_mins <- as.numeric(difftime(time_end, time_start, units = "mins"))
  n_intervals <- floor(n_mins / interval)
  
  vec_interval <- time_start + lubridate::minutes(seq(from = 0, by = interval, length.out = n_intervals))
  
  return(list(vec_interval = vec_interval,
              interval = interval,
              time_start = time_start,
              time_end = time_end))
  
}


#' Function to filter the data object based on the time period and intervals that
#' are needed for the files to be cut.
#' The function also creates identical Empatica E4 zipfiles in the same directory as
#' where the original zipfile is located.
#' @param data Object read with \code{\link{read_e4}}
#' @param time User input start time in the character format 
#' "yyyy-mm-dd hh:mm:ss" / e.g., "2019-11-27 08:32:00". Where do you want the file cut to start?
#' @param period User input period (in minutes) e.g., 30 or 60 minutes. What is the duration of the period (in minutes) that you want to split up?   
#' @param interval # Interval: User input interval (in minutes/ e.g., 5)
#' What is the duration of the interval you want to divide the period into? 
#' For example, the paper by de Looff et al. (2019) uses 5 minute intervals over
#' a 30 minute period preceding aggressive behavior. The 5 minute interval is 
#' chosen as for the calculation of some of the heart rate variability parameters 
#' one needs at least 5 minutes of data. 
#' @export
filter_createdir_zip <- function(data, time_start, time_end, interval, out_path = NULL){
  
  # Create an out object with the start times of the intervals needed
  out <- e4_filecut_intervals(time_start, time_end, interval)
  
  # Iterate over the intervals needed to be split    
  for (i in seq_along(out$vec_interval)){
    
    # Create a directory that uses the name of the original file and
    # then adds the interval number
    out_path <- file.path(paste0(attributes(data)[3][[1]],"_", i))
    dir.create(out_path, showWarnings = TRUE)  
    
    # What are the start and end times of the intervals needed? 
    # charclock could be entered here 
    start <- lubridate::ymd_hms(out$vec_interval[i], tz = Sys.timezone())
    end <- lubridate::ymd_hms(out$vec_interval[i], tz = Sys.timezone()) + lubridate::minutes(interval)
    
    # Filter the data
    data_filtered <- structure(
      list(
        
        EDA = dplyr::filter(data$EDA, 
                            .data$DateTime >= start,
                            .data$DateTime <= end),
        ACC = dplyr::filter(data$ACC, 
                            .data$DateTime >= start,
                            .data$DateTime <= end),
        TEMP = dplyr::filter(data$TEMP, 
                             .data$DateTime >= start,
                             .data$DateTime <= end),
        HR = dplyr::filter(data$HR, 
                           .data$DateTime >= start,
                           .data$DateTime <= end),
        BVP = dplyr::filter(data$BVP, 
                            .data$DateTime >= start,
                            .data$DateTime <= end),
        IBI = dplyr::filter(data$IBI, 
                            .data$DateTime >= start,
                            .data$DateTime <= end)
      ),
      class = "e4data",
      zipfile = attributes(data)$zipfile,
      tz = attributes(data)$tz
    )
    
    
    # And write it as unix (for the zip files)
    unix_e4 <- as.numeric(lubridate::with_tz(lubridate::ymd_hms(out$vec_interval[i],
                                                                tz = Sys.timezone()), tzone = "UTC"))
    
    # IBI has a different structure than the other files
    # IBI.csv has a column with the number of seconds since the start of the original recording
    # Therefore we use the unix time of the EDA file to ensure that the proper DateTime intervals
    # are selected and saved.
    unix_ibi <- as.numeric(lubridate::with_tz(lubridate::ymd_hms(data$EDA$DateTime[1],
                                                                 tz = Sys.timezone()), tzone = "UTC"))
    
    # Hz from the datafiles
    acc_hz <- 32
    bvp_hz <- 64
    eda_hz <- 4
    hr_hz <- 1
    temp_hz <- 4
    
    # Write the file to the created directory
    # EDA
    write.table(c(unix_e4, eda_hz, data_filtered$EDA$EDA),
                file = paste0(out_path, "/", "EDA.csv"), 
                quote=F, dec=".", row.names=FALSE, col.names=FALSE)
    
    # ACC
    write.table(data.frame(x = c(unix_e4, acc_hz, data_filtered$ACC$x),
                           y = c(unix_e4, acc_hz, data_filtered$ACC$y),
                           z = c(unix_e4, acc_hz, data_filtered$ACC$z)),
                file = paste0(out_path, "/", "ACC.csv"), 
                quote=F, sep = ",", dec=".", row.names=FALSE, col.names=FALSE)
    
    # TEMP
    write.table(c(unix_e4, temp_hz, data_filtered$TEMP$TEMP),
                file = paste0(out_path, "/", "TEMP.csv"), 
                quote=F, dec=".", row.names=FALSE, col.names=FALSE)
    
    # HR
    write.table(c(unix_e4, hr_hz, data_filtered$HR$HR),
                file = paste0(out_path, "/", "HR.csv"), 
                quote=F, dec=".", row.names=FALSE, col.names=FALSE)
    
    # BVP
    write.table(c(unix_e4, bvp_hz, data_filtered$BVP$BVP),
                file = paste0(out_path, "/", "BVP.csv"), 
                quote=F, dec=".", row.names=FALSE, col.names=FALSE)
    
    # IBI
    write.table(data.frame(time = c(unix_ibi, data_filtered$IBI$seconds),
                           ibi = c("IBI", data_filtered$IBI$IBI)),
                file = paste0(out_path, "/", "IBI.csv"), 
                quote=F, sep = ",", dec=".", row.names=FALSE, col.names=FALSE)
    
    # Zip and clean before end of sequence
    files2zip <- dir(out_path, full.names = TRUE)
    utils::zip(zipfile = out_path, files = files2zip, extras = '-j')
    
    unlink(out_path, recursive = TRUE)
    
  }
}


